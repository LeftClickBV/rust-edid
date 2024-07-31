use nom::{
    bytes::complete::{tag, take},
    combinator::{map, peek},
    multi::count,
    number::complete::{be_u16, le_u16, le_u32, le_u8},
    sequence::Tuple,
    IResult,
};

mod cp437;

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Header {
    pub vendor: [char; 3],
    pub product: u16,
    pub serial: u32,
    pub week: u8,
    pub year: u8, // Starting at year 1990
    pub version: u8,
    pub revision: u8,
}

fn parse_vendor(v: u16) -> [char; 3] {
    let mask: u8 = 0x1F; // Each letter is 5 bits
    let i0 = b'A' - 1; // 0x01 = A
    [
        (((v >> 10) as u8 & mask) + i0) as char,
        (((v >> 5) as u8 & mask) + i0) as char,
        ((v as u8 & mask) + i0) as char,
    ]
}

fn parse_header(i: &[u8]) -> IResult<&[u8], Header> {
    let pattern = tag(&[0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x00][..]);

    let (input, (_, vendor, product, serial, week, year, version, revision)) =
        (pattern, be_u16, le_u16, le_u32, le_u8, le_u8, le_u8, le_u8).parse(i)?;
    let header = Header {
        vendor: parse_vendor(vendor),
        product,
        serial,
        week,
        year,
        version,
        revision,
    };

    Ok((input, header))
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Display {
    pub video_input: u8,
    pub width: u8,  // cm
    pub height: u8, // cm
    pub gamma: u8,  // datavalue = (gamma*100)-100 (range 1.00–3.54)
    pub features: u8,
}

fn parse_display(i: &[u8]) -> IResult<&[u8], Display> {
    let (input, (video_input, width, height, gamma, features)) =
        (le_u8, le_u8, le_u8, le_u8, le_u8).parse(i)?;
    let display = Display {
        video_input,
        width,
        height,
        gamma,
        features,
    };

    Ok((input, display))
}

fn parse_chromaticity(i: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = take(10usize)(i)?;

    Ok((input, ()))
}

fn parse_established_timing(i: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = take(3usize)(i)?;

    Ok((input, ()))
}

fn parse_standard_timing(i: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = take(16usize)(i)?;

    Ok((input, ()))
}

fn parse_descriptor_text(i: &[u8]) -> IResult<&[u8], String> {
    let desc_text = map(take(13usize), |b: &[u8]| {
        b.iter()
            .filter(|c| **c != 0x0A)
            .map(|b| cp437::forward(*b))
            .collect::<String>()
    });

    map(desc_text, |s: String| s.trim().to_string())(i)
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct DetailedTiming {
    /// Pixel clock in kHz.
    pub pixel_clock: u32,
    pub horizontal_active_pixels: u16,
    pub horizontal_blanking_pixels: u16,
    pub vertical_active_lines: u16,
    pub vertical_blanking_lines: u16,
    pub horizontal_front_porch: u16,
    pub horizontal_sync_width: u16,
    pub vertical_front_porch: u16,
    pub vertical_sync_width: u16,
    /// Horizontal size in millimeters
    pub horizontal_size: u16,
    /// Vertical size in millimeters
    pub vertical_size: u16,
    /// Border pixels on one side of screen (i.e. total number is twice this)
    pub horizontal_border_pixels: u8,
    /// Border pixels on one side of screen (i.e. total number is twice this)
    pub vertical_border_pixels: u8,
    pub features: u8, /* TODO add enums etc. */
}

fn parse_detailed_timing(i: &[u8]) -> IResult<&[u8], DetailedTiming> {
    let (
        input,
        (
            pixel_clock_10khz,
            horizontal_active_lo,
            horizontal_blanking_lo,
            horizontal_px_hi,
            vertical_active_lo,
            vertical_blanking_lo,
            vertical_px_hi,
            horizontal_front_porch_lo,
            horizontal_sync_width_lo,
            vertical_lo,
            porch_sync_hi,
            horizontal_size_lo,
            vertical_size_lo,
            size_hi,
            horizontal_border,
            vertical_border,
            features,
        ),
    ) = (
        le_u16, le_u8, le_u8, le_u8, le_u8, le_u8, le_u8, le_u8, le_u8, le_u8, le_u8, le_u8, le_u8,
        le_u8, le_u8, le_u8, le_u8,
    )
        .parse(i)?;
    let detailed_timing = DetailedTiming {
        pixel_clock: pixel_clock_10khz as u32 * 10,
        horizontal_active_pixels: (horizontal_active_lo as u16)
            | (((horizontal_px_hi >> 4) as u16) << 8),
        horizontal_blanking_pixels: (horizontal_blanking_lo as u16)
            | (((horizontal_px_hi & 0xf) as u16) << 8),
        vertical_active_lines: (vertical_active_lo as u16) | (((vertical_px_hi >> 4) as u16) << 8),
        vertical_blanking_lines: (vertical_blanking_lo as u16)
            | (((vertical_px_hi & 0xf) as u16) << 8),
        horizontal_front_porch: (horizontal_front_porch_lo as u16)
            | (((porch_sync_hi >> 6) as u16) << 8),
        horizontal_sync_width: (horizontal_sync_width_lo as u16)
            | ((((porch_sync_hi >> 4) & 0x3) as u16) << 8),
        vertical_front_porch: ((vertical_lo >> 4) as u16)
            | ((((porch_sync_hi >> 2) & 0x3) as u16) << 8),
        vertical_sync_width: ((vertical_lo & 0xf) as u16) | (((porch_sync_hi & 0x3) as u16) << 8),
        horizontal_size: (horizontal_size_lo as u16) | (((size_hi >> 4) as u16) << 8),
        vertical_size: (vertical_size_lo as u16) | (((size_hi & 0xf) as u16) << 8),
        horizontal_border_pixels: horizontal_border,
        vertical_border_pixels: vertical_border,
        features,
    };

    Ok((input, detailed_timing))
}

#[derive(Debug, PartialEq, Clone)]
pub enum Descriptor {
    DetailedTiming(DetailedTiming),
    SerialNumber(String),
    UnspecifiedText(String),
    RangeLimits, // TODO
    ProductName(String),
    WhitePoint,     // TODO
    StandardTiming, // TODO
    ColorManagement,
    TimingCodes,
    EstablishedTimings,
    Dummy,
    Unknown(Vec<u8>),
}

fn parse_descriptor(i: &[u8]) -> IResult<&[u8], Descriptor> {
    let (_, desc) = peek(le_u16)(i)?;

    if desc == 0 {
        let (i, (_, desc_type, _)) = (take(3usize), le_u8, take(1usize)).parse(i)?;

        match desc_type {
            0xFF => {
                let (input, serial_number) = parse_descriptor_text(i)?;

                Ok((input, Descriptor::SerialNumber(serial_number)))
            }
            0xFE => {
                let (input, text) = parse_descriptor_text(i)?;

                Ok((input, Descriptor::UnspecifiedText(text)))
            }
            0xFD => {
                let (input, _) = take(13usize)(i)?;

                Ok((input, Descriptor::RangeLimits))
            }
            0xFC => {
                let (input, prod_name) = parse_descriptor_text(i)?;

                Ok((input, Descriptor::ProductName(prod_name)))
            }
            0xFB => {
                let (input, _) = take(13usize)(i)?;

                Ok((input, Descriptor::WhitePoint))
            }
            0xFA => {
                let (input, _) = take(13usize)(i)?;

                Ok((input, Descriptor::StandardTiming))
            }
            0xF9 => {
                let (input, _) = take(13usize)(i)?;

                Ok((input, Descriptor::ColorManagement))
            }
            0xF8 => {
                let (input, _) = take(13usize)(i)?;

                Ok((input, Descriptor::TimingCodes))
            }
            0xF7 => {
                let (input, _) = take(13usize)(i)?;

                Ok((input, Descriptor::EstablishedTimings))
            }
            0x10 => {
                let (input, _) = take(13usize)(i)?;

                Ok((input, Descriptor::Dummy))
            }
            _ => {
                let (input, data) = count(le_u8, 13)(i)?;

                Ok((input, Descriptor::Unknown(data)))
            }
        }
    } else {
        let (input, d) = parse_detailed_timing(i)?;

        Ok((input, Descriptor::DetailedTiming(d)))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct EDID {
    pub header: Header,
    pub display: Display,
    chromaticity: (),       // TODO
    established_timing: (), // TODO
    standard_timing: (),    // TODO
    pub descriptors: Vec<Descriptor>,
}

pub fn parse(data: &[u8]) -> IResult<&[u8], EDID> {
    let (
        input,
        (header, display, chromaticity, established_timing, standard_timing, descriptors, _, _),
    ) = (
        parse_header,
        parse_display,
        parse_chromaticity,
        parse_established_timing,
        parse_standard_timing,
        count(parse_descriptor, 4),
        take(1usize),
        take(1usize),
    )
        .parse(data)?;

    Ok((
        input,
        EDID {
            header,
            display,
            chromaticity,
            established_timing,
            standard_timing,
            descriptors,
        },
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test(d: &[u8], expected: &EDID) {
        match parse(d) {
            Ok((remaining, parsed)) => {
                assert_eq!(remaining.len(), 0);
                assert_eq!(&parsed, expected);
            }
            Err(nom::Err::Incomplete(_)) => {
                panic!("Incomplete");
            }
            Err(nom::Err::Error(err) | nom::Err::Failure(err)) => {
                panic!("{:?}", err);
            }
        }
    }

    #[test]
    fn test_card0_vga_1() {
        let d = include_bytes!("../testdata/card0-VGA-1");

        let expected = EDID {
            header: Header {
                vendor: ['S', 'A', 'M'],
                product: 596,
                serial: 1146106418,
                week: 27,
                year: 17,
                version: 1,
                revision: 3,
            },
            display: Display {
                video_input: 14,
                width: 47,
                height: 30,
                gamma: 120,
                features: 42,
            },
            chromaticity: (),
            established_timing: (),
            standard_timing: (),
            descriptors: vec![
                Descriptor::DetailedTiming(DetailedTiming {
                    pixel_clock: 146250,
                    horizontal_active_pixels: 1680,
                    horizontal_blanking_pixels: 560,
                    vertical_active_lines: 1050,
                    vertical_blanking_lines: 39,
                    horizontal_front_porch: 104,
                    horizontal_sync_width: 176,
                    vertical_front_porch: 3,
                    vertical_sync_width: 6,
                    horizontal_size: 474,
                    vertical_size: 296,
                    horizontal_border_pixels: 0,
                    vertical_border_pixels: 0,
                    features: 28,
                }),
                Descriptor::RangeLimits,
                Descriptor::ProductName("SyncMaster".to_string()),
                Descriptor::SerialNumber("HS3P701105".to_string()),
            ],
        };

        test(d, &expected);
    }

    #[test]
    fn test_card0_edp_1() {
        let d = include_bytes!("../testdata/card0-eDP-1");

        let expected = EDID {
            header: Header {
                vendor: ['S', 'H', 'P'],
                product: 5193,
                serial: 0,
                week: 32,
                year: 25,
                version: 1,
                revision: 4,
            },
            display: Display {
                video_input: 165,
                width: 29,
                height: 17,
                gamma: 120,
                features: 14,
            },
            chromaticity: (),
            established_timing: (),
            standard_timing: (),
            descriptors: vec![
                Descriptor::DetailedTiming(DetailedTiming {
                    pixel_clock: 138500,
                    horizontal_active_pixels: 1920,
                    horizontal_blanking_pixels: 160,
                    vertical_active_lines: 1080,
                    vertical_blanking_lines: 31,
                    horizontal_front_porch: 48,
                    horizontal_sync_width: 32,
                    vertical_front_porch: 3,
                    vertical_sync_width: 5,
                    horizontal_size: 294,
                    vertical_size: 165,
                    horizontal_border_pixels: 0,
                    vertical_border_pixels: 0,
                    features: 24,
                }),
                Descriptor::Dummy,
                Descriptor::UnspecifiedText("DJCP6ÇLQ133M1".to_string()),
                Descriptor::Unknown(vec![2, 65, 3, 40, 0, 18, 0, 0, 11, 1, 10, 32, 32]),
            ],
        };

        test(d, &expected);
    }
}
