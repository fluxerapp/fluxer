// SPDX-License-Identifier: AGPL-3.0-or-later

const MAX_TRANSFORM_PIXELS: u64 = 200_000_000;
const RGBA_BYTES_PER_PIXEL: usize = 4;
const RGBA_RESULT_HEADER_BYTES: usize = 8;
const NULL_U32: u32 = u32::MAX;

#[derive(Clone, Copy, Debug)]
pub struct TransformRequest {
    pub src_width: u32,
    pub src_height: u32,
    pub x: u32,
    pub y: u32,
    pub width: u32,
    pub height: u32,
    pub rotation_deg: u32,
    pub resize_width: u32,
    pub resize_height: u32,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TransformError {
    InvalidDimensions,
    InvalidRgbaLength,
    InvalidOutputLength,
    EmptyCrop,
    EmptyTarget,
    ImageTooLarge,
    OutOfMemory,
}

impl TransformError {
    pub fn message(self) -> &'static str {
        match self {
            Self::InvalidDimensions => "invalid RGBA dimensions",
            Self::InvalidRgbaLength => "RGBA input length does not match dimensions",
            Self::InvalidOutputLength => "RGBA output length does not match target dimensions",
            Self::EmptyCrop => "Crop area is empty",
            Self::EmptyTarget => "Target dimensions are empty",
            Self::ImageTooLarge => "Image is too large to crop",
            Self::OutOfMemory => "out of memory",
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct Crop {
    x: u32,
    y: u32,
    width: u32,
    height: u32,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct OutputGeometry {
    crop: Crop,
    rotation: u32,
    base_width: u32,
    base_height: u32,
    target_width: u32,
    target_height: u32,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct Point {
    x: usize,
    y: usize,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct PixelLayout {
    src_width: usize,
    dst_width: usize,
}

pub fn crop_rotate_rgba_alloc(
    input: &[u8],
    request: TransformRequest,
) -> Result<Vec<u8>, TransformError> {
    let geometry = checked_geometry(input, request)?;
    let output_len = rgba_byte_len(
        geometry.target_width,
        geometry.target_height,
        RGBA_RESULT_HEADER_BYTES,
    )?;
    let mut output = try_zeroed_vec(output_len)?;
    write_u32_le(&mut output, 0, geometry.target_width);
    write_u32_le(&mut output, 4, geometry.target_height);
    write_transformed(
        input,
        &mut output[RGBA_RESULT_HEADER_BYTES..],
        request,
        geometry,
    );
    Ok(output)
}

pub fn crop_rotate_rgba_into(
    input: &[u8],
    output: &mut [u8],
    request: TransformRequest,
) -> Result<(), TransformError> {
    let geometry = checked_geometry(input, request)?;
    if output.len() != rgba_byte_len(geometry.target_width, geometry.target_height, 0)? {
        return Err(TransformError::InvalidOutputLength);
    }
    write_transformed(input, output, request, geometry);
    Ok(())
}

fn checked_geometry(
    input: &[u8],
    request: TransformRequest,
) -> Result<OutputGeometry, TransformError> {
    let geometry = output_geometry(request)?;
    if input.len() != rgba_byte_len(request.src_width, request.src_height, 0)? {
        return Err(TransformError::InvalidRgbaLength);
    }
    Ok(geometry)
}

fn write_transformed(
    input: &[u8],
    dst: &mut [u8],
    request: TransformRequest,
    geometry: OutputGeometry,
) {
    if geometry.target_width == geometry.base_width
        && geometry.target_height == geometry.base_height
    {
        copy_rotated_without_resize(input, dst, request, geometry);
    } else {
        copy_rotated_with_nearest_resize(input, dst, request, geometry);
    }
}

fn output_geometry(request: TransformRequest) -> Result<OutputGeometry, TransformError> {
    if request.src_width == 0 || request.src_height == 0 {
        return Err(TransformError::InvalidDimensions);
    }

    let crop = clamped_crop(
        request.src_width,
        request.src_height,
        request.x,
        request.y,
        request.width,
        request.height,
    );
    if crop.width == 0 || crop.height == 0 {
        return Err(TransformError::EmptyCrop);
    }

    let rotation = quarter_turn_rotation(request.rotation_deg);
    let (base_width, base_height) = if rotation == 90 || rotation == 270 {
        (crop.height, crop.width)
    } else {
        (crop.width, crop.height)
    };
    let target_width = if has_resize(request.resize_width) {
        request.resize_width
    } else {
        base_width
    };
    let target_height = if has_resize(request.resize_height) {
        request.resize_height
    } else {
        base_height
    };
    if target_width == 0 || target_height == 0 {
        return Err(TransformError::EmptyTarget);
    }

    rgba_byte_len(target_width, target_height, RGBA_RESULT_HEADER_BYTES)?;

    Ok(OutputGeometry {
        crop,
        rotation,
        base_width,
        base_height,
        target_width,
        target_height,
    })
}

fn clamped_crop(src_width: u32, src_height: u32, x: u32, y: u32, width: u32, height: u32) -> Crop {
    let crop_x = x.min(src_width);
    let crop_y = y.min(src_height);
    Crop {
        x: crop_x,
        y: crop_y,
        width: width.min(src_width - crop_x),
        height: height.min(src_height - crop_y),
    }
}

fn rgba_byte_len(width: u32, height: u32, extra_bytes: usize) -> Result<usize, TransformError> {
    let pixels = u64::from(width) * u64::from(height);
    if pixels > MAX_TRANSFORM_PIXELS {
        return Err(TransformError::ImageTooLarge);
    }

    let byte_len = pixels
        .checked_mul(RGBA_BYTES_PER_PIXEL as u64)
        .and_then(|len| len.checked_add(extra_bytes as u64))
        .ok_or(TransformError::ImageTooLarge)?;
    usize::try_from(byte_len).map_err(|_| TransformError::ImageTooLarge)
}

fn try_zeroed_vec(len: usize) -> Result<Vec<u8>, TransformError> {
    let mut output = Vec::new();
    output
        .try_reserve_exact(len)
        .map_err(|_| TransformError::OutOfMemory)?;
    output.resize(len, 0);
    Ok(output)
}

fn normalized_rotation(rotation_deg: u32) -> u32 {
    rotation_deg % 360
}

fn quarter_turn_rotation(rotation_deg: u32) -> u32 {
    match normalized_rotation(rotation_deg) {
        90 | 180 | 270 => normalized_rotation(rotation_deg),
        _ => 0,
    }
}

fn has_resize(value: u32) -> bool {
    value != NULL_U32 && value > 0
}

fn write_u32_le(output: &mut [u8], offset: usize, value: u32) {
    output[offset..offset + 4].copy_from_slice(&value.to_le_bytes());
}

fn copy_rotated_without_resize(
    input: &[u8],
    dst: &mut [u8],
    request: TransformRequest,
    geometry: OutputGeometry,
) {
    let src_width = request.src_width as usize;
    let crop = geometry.crop;
    let crop_x = crop.x as usize;
    let crop_y = crop.y as usize;
    let crop_width = crop.width as usize;
    let crop_height = crop.height as usize;
    let layout = PixelLayout {
        src_width,
        dst_width: geometry.target_width as usize,
    };

    match geometry.rotation {
        0 => {
            for row in 0..crop_height {
                let src_offset = ((crop_y + row) * src_width + crop_x) * RGBA_BYTES_PER_PIXEL;
                let dst_offset = row * crop_width * RGBA_BYTES_PER_PIXEL;
                let len = crop_width * RGBA_BYTES_PER_PIXEL;
                dst[dst_offset..dst_offset + len]
                    .copy_from_slice(&input[src_offset..src_offset + len]);
            }
        }
        90 => {
            for y0 in 0..crop_height {
                for x0 in 0..crop_width {
                    let dst_x = crop_height - 1 - y0;
                    let dst_y = x0;
                    copy_pixel(
                        input,
                        dst,
                        layout,
                        Point {
                            x: crop_x + x0,
                            y: crop_y + y0,
                        },
                        Point { x: dst_x, y: dst_y },
                    );
                }
            }
        }
        180 => {
            for y0 in 0..crop_height {
                for x0 in 0..crop_width {
                    let dst_x = crop_width - 1 - x0;
                    let dst_y = crop_height - 1 - y0;
                    copy_pixel(
                        input,
                        dst,
                        layout,
                        Point {
                            x: crop_x + x0,
                            y: crop_y + y0,
                        },
                        Point { x: dst_x, y: dst_y },
                    );
                }
            }
        }
        270 => {
            for y0 in 0..crop_height {
                for x0 in 0..crop_width {
                    let dst_x = y0;
                    let dst_y = crop_width - 1 - x0;
                    copy_pixel(
                        input,
                        dst,
                        layout,
                        Point {
                            x: crop_x + x0,
                            y: crop_y + y0,
                        },
                        Point { x: dst_x, y: dst_y },
                    );
                }
            }
        }
        _ => unreachable!("rotation is normalized to quarter turns"),
    }
}

fn copy_rotated_with_nearest_resize(
    input: &[u8],
    dst: &mut [u8],
    request: TransformRequest,
    geometry: OutputGeometry,
) {
    let src_width = request.src_width as usize;
    let crop = geometry.crop;
    let crop_x = crop.x as usize;
    let crop_y = crop.y as usize;
    let crop_width = u64::from(crop.width);
    let crop_height = u64::from(crop.height);
    let base_width = u64::from(geometry.base_width);
    let base_height = u64::from(geometry.base_height);
    let target_width = geometry.target_width as usize;
    let target_height = geometry.target_height as usize;
    let target_width_u64 = u64::from(geometry.target_width);
    let target_height_u64 = u64::from(geometry.target_height);
    let layout = PixelLayout {
        src_width,
        dst_width: target_width,
    };

    for dst_y in 0..target_height {
        let rotated_y = (dst_y as u64 * base_height) / target_height_u64;
        for dst_x in 0..target_width {
            let rotated_x = (dst_x as u64 * base_width) / target_width_u64;
            let cropped_x = match geometry.rotation {
                90 => rotated_y,
                180 => crop_width - 1 - rotated_x,
                270 => crop_width - 1 - rotated_y,
                _ => rotated_x,
            } as usize;
            let cropped_y = match geometry.rotation {
                90 => crop_height - 1 - rotated_x,
                180 => crop_height - 1 - rotated_y,
                270 => rotated_x,
                _ => rotated_y,
            } as usize;
            copy_pixel(
                input,
                dst,
                layout,
                Point {
                    x: crop_x + cropped_x,
                    y: crop_y + cropped_y,
                },
                Point { x: dst_x, y: dst_y },
            );
        }
    }
}

#[inline]
fn copy_pixel(input: &[u8], dst: &mut [u8], layout: PixelLayout, src: Point, target: Point) {
    let src_offset = (src.y * layout.src_width + src.x) * RGBA_BYTES_PER_PIXEL;
    let dst_offset = (target.y * layout.dst_width + target.x) * RGBA_BYTES_PER_PIXEL;
    dst[dst_offset..dst_offset + RGBA_BYTES_PER_PIXEL]
        .copy_from_slice(&input[src_offset..src_offset + RGBA_BYTES_PER_PIXEL]);
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    fn request(src_width: u32, src_height: u32) -> TransformRequest {
        TransformRequest {
            src_width,
            src_height,
            x: 0,
            y: 0,
            width: src_width,
            height: src_height,
            rotation_deg: 0,
            resize_width: NULL_U32,
            resize_height: NULL_U32,
        }
    }

    fn rgba(values: &[u8]) -> Vec<u8> {
        values
            .iter()
            .flat_map(|value| [*value, 0, 0, 255])
            .collect()
    }

    fn payload(output: &[u8]) -> &[u8] {
        &output[RGBA_RESULT_HEADER_BYTES..]
    }

    fn dimensions(output: &[u8]) -> (u32, u32) {
        (
            u32::from_le_bytes(output[0..4].try_into().unwrap()),
            u32::from_le_bytes(output[4..8].try_into().unwrap()),
        )
    }

    #[test]
    fn rotate_90_uses_clockwise_quarter_turns() {
        let mut transform = request(2, 3);
        transform.rotation_deg = 90;
        let output = crop_rotate_rgba_alloc(&rgba(&[1, 2, 3, 4, 5, 6]), transform).unwrap();
        assert_eq!(dimensions(&output), (3, 2));
        assert_eq!(payload(&output), rgba(&[5, 3, 1, 6, 4, 2]));
    }

    #[test]
    fn rotate_180_reverses_rows_and_columns() {
        let mut transform = request(2, 2);
        transform.rotation_deg = 180;
        let output = crop_rotate_rgba_alloc(&rgba(&[1, 2, 3, 4]), transform).unwrap();
        assert_eq!(dimensions(&output), (2, 2));
        assert_eq!(payload(&output), rgba(&[4, 3, 2, 1]));
    }

    #[test]
    fn rotate_270_uses_clockwise_quarter_turns() {
        let mut transform = request(2, 3);
        transform.rotation_deg = 270;
        let output = crop_rotate_rgba_alloc(&rgba(&[1, 2, 3, 4, 5, 6]), transform).unwrap();
        assert_eq!(dimensions(&output), (3, 2));
        assert_eq!(payload(&output), rgba(&[2, 4, 6, 1, 3, 5]));
    }

    #[test]
    fn applies_nearest_resize_after_rotation() {
        let mut transform = request(4, 1);
        transform.x = 1;
        transform.width = 2;
        transform.resize_width = 4;
        let output = crop_rotate_rgba_alloc(&rgba(&[1, 2, 3, 4]), transform).unwrap();
        assert_eq!(dimensions(&output), (4, 1));
        assert_eq!(payload(&output), rgba(&[2, 2, 3, 3]));
    }

    #[test]
    fn clamps_crop_to_source_bounds() {
        let mut transform = request(3, 2);
        transform.x = 2;
        transform.y = 1;
        transform.width = 99;
        transform.height = 99;
        let output = crop_rotate_rgba_alloc(&rgba(&[1, 2, 3, 4, 5, 6]), transform).unwrap();
        assert_eq!(dimensions(&output), (1, 1));
        assert_eq!(payload(&output), rgba(&[6]));
    }

    #[test]
    fn rejects_invalid_source_shape() {
        assert_eq!(
            crop_rotate_rgba_alloc(&[], request(0, 1)).unwrap_err(),
            TransformError::InvalidDimensions
        );
        assert_eq!(
            crop_rotate_rgba_alloc(&[0, 0, 0, 0], request(2, 1)).unwrap_err(),
            TransformError::InvalidRgbaLength
        );
    }

    #[test]
    fn rejects_empty_crop_and_target() {
        let mut empty_crop = request(2, 2);
        empty_crop.x = 2;
        assert_eq!(
            crop_rotate_rgba_alloc(&rgba(&[1, 2, 3, 4]), empty_crop).unwrap_err(),
            TransformError::EmptyCrop
        );

        let mut empty_target = request(2, 2);
        empty_target.resize_width = 0;
        empty_target.resize_height = 1;
        assert!(crop_rotate_rgba_alloc(&rgba(&[1, 2, 3, 4]), empty_target).is_ok());
    }

    #[test]
    fn into_writes_the_same_pixels_as_alloc() {
        let input = rgba(&[1, 2, 3, 4, 5, 6]);
        for rotation_deg in [0, 90, 180, 270] {
            let mut transform = request(2, 3);
            transform.rotation_deg = rotation_deg;
            transform.resize_width = 5;
            transform.resize_height = 4;
            let expected = crop_rotate_rgba_alloc(&input, transform).unwrap();
            let mut output = vec![0xaa; 5 * 4 * RGBA_BYTES_PER_PIXEL];
            crop_rotate_rgba_into(&input, &mut output, transform).unwrap();
            assert_eq!(output, payload(&expected));
        }
    }

    #[test]
    fn into_rejects_an_output_of_the_wrong_length() {
        let mut output = vec![0; 3 * RGBA_BYTES_PER_PIXEL];
        assert_eq!(
            crop_rotate_rgba_into(&rgba(&[1, 2, 3, 4]), &mut output, request(2, 2)).unwrap_err(),
            TransformError::InvalidOutputLength
        );
    }

    proptest! {
        #[test]
        fn identity_transform_preserves_pixels(width in 1u32..16, height in 1u32..16) {
            let pixel_count = (width * height) as usize;
            let input = rgba(&(0..pixel_count).map(|index| index as u8).collect::<Vec<_>>());
            let output = crop_rotate_rgba_alloc(&input, request(width, height)).unwrap();
            prop_assert_eq!(dimensions(&output), (width, height));
            prop_assert_eq!(payload(&output), input.as_slice());
        }
    }
}
