// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {AVIF_FORMAT_LABEL} from '@app/features/app/config/I18nDisplayConstants';
import {inspectImageBytes} from '@app/features/expressions/utils/AnimatedImageUtils';
import type {AnimatedCropSourceFormat, CropParams} from '@app/features/expressions/workers/AnimatedImageCropMessages';
import {CropPipelineError, cropAnimatedImage} from '@app/features/expressions/workers/AnimatedImageCropWorkerManager';
import {showMessagingErrorModal} from '@app/features/messaging/components/alerts/MessagingErrorModalUtils';
import styles from '@app/features/messaging/components/modals/ImageCropModal.module.css';
import {formatFileSize} from '@app/features/messaging/utils/FileUtils';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {Button} from '@app/features/ui/button/Button';
import {Slider} from '@app/features/ui/components/Slider';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {usePopOwningModal} from '@app/features/ui/utils/ModalUtils';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {ArrowClockwiseIcon, ImageSquareIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';

const CROP_PREVIEW_DESCRIPTOR = msg({
	message: 'Crop preview',
	comment: 'Short label in the image crop modal. Keep it concise.',
});
const ROTATE_CLOCKWISE_DESCRIPTOR = msg({
	message: 'Rotate clockwise',
	comment: 'Short label in the image crop modal. Keep it concise.',
});
const COULDN_T_CROP_IMAGE_DESCRIPTOR = msg({
	message: "Couldn't crop image",
	comment: 'Title of the error modal shown when image cropping fails.',
});
const CROPPED_IMAGE_IS_TOO_LARGE_PLEASE_CHOOSE_A_DESCRIPTOR = msg({
	message: 'Cropped image is too large. Choose a smaller area or a smaller file (max {maxSizeLabel}).',
	comment:
		'Error modal body shown when a cropped image still exceeds the size limit. {maxSizeLabel} is a formatted file size.',
});
const ANIMATION_TOO_LONG_OR_TOO_LARGE_DESCRIPTOR = msg({
	message: 'This animation is too long or too large to crop. Try a shorter or smaller file.',
	comment: 'Error modal body shown when an animated image has too many frames or pixels to crop in the browser.',
});
const IMAGE_COULDN_T_BE_READ_DESCRIPTOR = msg({
	message: "This image couldn't be read. It may be damaged.",
	comment: 'Error modal body shown when an animated image fails to decode during cropping.',
});
const BROWSER_CAN_T_CROP_ANIMATED_AVIF_DESCRIPTOR = msg({
	message: "This browser can't crop animated {avifFormatLabel} files. Upload a GIF or WebP instead.",
	comment:
		'Error modal body shown when the browser cannot decode animated AVIF for cropping. {avifFormatLabel} is the AVIF format name.',
});
const WEBASSEMBLY_TURNED_OFF_DESCRIPTOR = msg({
	message: "This browser has WebAssembly turned off, so animated images can't be cropped.",
	comment: 'Error modal body shown when WebAssembly is unavailable, which animated image cropping needs.',
});
const CROP_PROGRESS_PREPARING_DESCRIPTOR = msg({
	message: 'Preparing',
	comment: 'Progress label on the save button while an animated image crop starts. Keep it concise.',
});
const CROP_PROGRESS_CROPPING_DESCRIPTOR = msg({
	message: 'Cropping {percent}%',
	comment:
		'Progress label on the save button while animated image frames are cropped. {percent} is a whole number. Keep it concise.',
});
const CROP_PROGRESS_COMPRESSING_DESCRIPTOR = msg({
	message: 'Compressing',
	comment: 'Progress label on the save button while a cropped animated image is compressed. Keep it concise.',
});
const logger = new Logger('ImageCropModal');

interface Point {
	x: number;
	y: number;
}

interface Size {
	width: number;
	height: number;
}

interface DragBoundaries {
	top: number;
	bottom: number;
	left: number;
	right: number;
}

class CropSizeLimitError extends Error {
	readonly maxBytes: number;

	constructor(actualBytes: number, maxBytes: number) {
		super(`Cropped image is ${actualBytes} bytes and exceeds the limit of ${maxBytes} bytes`);
		this.name = 'CropSizeLimitError';
		this.maxBytes = maxBytes;
	}
}

interface CropProgress {
	phase: 'init' | 'crop' | 'compress';
	fraction: number;
}

function isAnimatedCropSourceFormat(format: string): format is AnimatedCropSourceFormat {
	return format === 'gif' || format === 'png' || format === 'webp' || format === 'avif';
}

function toCropRotation(rotationDeg: number): CropParams['rotation'] {
	const rot = ((rotationDeg % 360) + 360) % 360;
	if (rot === 90 || rot === 180 || rot === 270) return rot;
	return 0;
}

function clamp(value: number, min: number, max: number): number {
	if (value < min) return min;
	if (value > max) return max;
	return value;
}

function inRange(value: number, start: number, end?: number): boolean {
	if (end === undefined) {
		end = start;
		start = 0;
	}
	if (start > end) {
		const tmp = start;
		start = end;
		end = tmp;
	}
	return value >= start && value < end;
}

function computeCropRect(containerWidth: number, containerHeight: number, aspectRatio: number): Size {
	if (containerWidth <= 0 || containerHeight <= 0 || aspectRatio <= 0) {
		return {width: containerWidth, height: containerHeight};
	}
	let width = containerWidth;
	let height = width / aspectRatio;
	if (height > containerHeight) {
		height = containerHeight;
		width = height * aspectRatio;
	}
	return {width, height};
}

function computeDragBoundaries(imageWidth: number, imageHeight: number, cropRect: Size): DragBoundaries {
	const excessWidth = imageWidth - cropRect.width;
	const excessHeight = imageHeight - cropRect.height;
	const left = excessWidth !== 0 ? -Math.abs(excessWidth / 2) : 0;
	const right = excessWidth !== 0 ? excessWidth / 2 : 0;
	const bottom = excessHeight !== 0 ? -Math.abs(excessHeight / 2) : 0;
	const top = excessHeight !== 0 ? excessHeight / 2 : 0;
	return {top, bottom, left, right};
}

function clampTransformToBounds(x: number, y: number, bounds: DragBoundaries): Point {
	return {
		x: clamp(x, bounds.left, bounds.right),
		y: clamp(y, bounds.bottom, bounds.top),
	};
}

function rotatePoint({x, y}: Point, rotationDeg: number): Point {
	const rot = ((rotationDeg % 360) + 360) % 360;
	switch (rot) {
		case 90:
			return {x: y, y: -x};
		case 180:
			return {x: -x, y: -y};
		case 270:
			return {x: -y, y: x};
		default:
			return {x, y};
	}
}

function computeDestinationOffset(cropWidthNatural: number, cropHeightNatural: number, rotationDeg: number): Point {
	const rot = ((rotationDeg % 360) + 360) % 360;
	switch (rot) {
		case 0:
			return {x: 0, y: 0};
		case 90:
			return {x: 0, y: -cropWidthNatural};
		case 180:
			return {x: -cropWidthNatural, y: -cropHeightNatural};
		case 270:
			return {x: -cropHeightNatural, y: 0};
		default:
			return {x: 0, y: 0};
	}
}

interface ComputeCropGeometryInput {
	image: HTMLImageElement;
	displayDimensions: Size;
	cropDimensions: Size;
	cropOrigin: Point;
	maxDimensions: Size;
	rotationDeg?: number;
}

interface ComputeCropGeometryOutput {
	sourceX: number;
	sourceY: number;
	sourceWidth: number;
	sourceHeight: number;
	destinationX: number;
	destinationY: number;
	destinationWidth: number;
	destinationHeight: number;
	canvasWidth: number;
	canvasHeight: number;
}

function computeCropGeometry(input: ComputeCropGeometryInput): ComputeCropGeometryOutput {
	const {image, displayDimensions, cropDimensions, cropOrigin, maxDimensions, rotationDeg = 0} = input;
	const displayWidth = displayDimensions.width;
	const displayHeight = displayDimensions.height;
	const scale = image.naturalWidth / displayWidth;
	const rotatedOrigin = rotatePoint(cropOrigin, rotationDeg);
	const isRotated90Or270 = rotationDeg % 180 !== 0;
	const cropWidthNatural = cropDimensions.width * scale;
	const cropHeightNatural = cropDimensions.height * scale;
	const canvasWidth = Math.min(cropWidthNatural, maxDimensions.width);
	const canvasHeight = Math.min(cropHeightNatural, maxDimensions.height);
	const halfCropMain = (isRotated90Or270 ? cropDimensions.height : cropDimensions.width) / 2;
	const halfCropCross = (isRotated90Or270 ? cropDimensions.width : cropDimensions.height) / 2;
	const sourceX = (displayWidth / 2 - halfCropMain - rotatedOrigin.x) * scale;
	const sourceY = (displayHeight / 2 - halfCropCross - rotatedOrigin.y) * scale;
	const sourceWidth = isRotated90Or270 ? cropHeightNatural : cropWidthNatural;
	const sourceHeight = isRotated90Or270 ? cropWidthNatural : cropHeightNatural;
	let {x: destX, y: destY} = computeDestinationOffset(cropWidthNatural, cropHeightNatural, rotationDeg);
	if (maxDimensions.width < cropWidthNatural) {
		destX *= maxDimensions.width / cropWidthNatural;
	}
	if (maxDimensions.height < cropHeightNatural) {
		destY *= maxDimensions.height / cropHeightNatural;
	}
	const destinationWidth = isRotated90Or270 ? canvasHeight : canvasWidth;
	const destinationHeight = isRotated90Or270 ? canvasWidth : canvasHeight;
	return {
		sourceX,
		sourceY,
		sourceWidth,
		sourceHeight,
		destinationX: destX,
		destinationY: destY,
		destinationWidth,
		destinationHeight,
		canvasWidth,
		canvasHeight,
	};
}

function containSize(srcW: number, srcH: number, boxW: number, boxH: number): {w: number; h: number} {
	if (!(srcW > 0 && srcH > 0 && boxW > 0 && boxH > 0)) return {w: 0, h: 0};
	const scale = Math.min(boxW / srcW, boxH / srcH);
	const eff = Math.min(scale, 1);
	return {
		w: Math.max(1, Math.floor(srcW * eff)),
		h: Math.max(1, Math.floor(srcH * eff)),
	};
}

async function exportStaticImage(
	image: HTMLImageElement,
	displayDimensions: Size,
	cropDimensions: Size,
	cropOrigin: Point,
	rotationDeg: number,
	maxW: number,
	maxH: number,
	maxBytes: number,
): Promise<Blob> {
	if (!image.complete || image.naturalWidth === 0 || image.naturalHeight === 0) {
		throw new Error('Image not fully loaded');
	}
	const scale = image.naturalWidth / displayDimensions.width;
	const cropNativeWidth = cropDimensions.width * scale;
	const cropNativeHeight = cropDimensions.height * scale;
	const {w: targetW, h: targetH} = containSize(cropNativeWidth, cropNativeHeight, maxW, maxH);
	const geom = computeCropGeometry({
		image,
		displayDimensions,
		cropDimensions,
		cropOrigin,
		maxDimensions: {width: targetW, height: targetH},
		rotationDeg,
	});
	if (
		!Number.isFinite(geom.canvasWidth) ||
		!Number.isFinite(geom.canvasHeight) ||
		geom.canvasWidth <= 0 ||
		geom.canvasHeight <= 0
	) {
		throw new Error('Invalid canvas dimensions');
	}
	if (
		!Number.isFinite(geom.sourceWidth) ||
		!Number.isFinite(geom.sourceHeight) ||
		geom.sourceWidth <= 0 ||
		geom.sourceHeight <= 0
	) {
		throw new Error('Invalid source dimensions');
	}
	const canvas = document.createElement('canvas');
	canvas.width = Math.round(geom.canvasWidth);
	canvas.height = Math.round(geom.canvasHeight);
	const ctx = canvas.getContext('2d');
	if (!ctx) throw new Error('Failed to get canvas context');
	const clampedSourceX = Math.max(0, geom.sourceX);
	const clampedSourceY = Math.max(0, geom.sourceY);
	ctx.save();
	ctx.rotate((rotationDeg * Math.PI) / 180);
	ctx.drawImage(
		image,
		clampedSourceX,
		clampedSourceY,
		geom.sourceWidth,
		geom.sourceHeight,
		geom.destinationX,
		geom.destinationY,
		geom.destinationWidth,
		geom.destinationHeight,
	);
	ctx.restore();
	const blob: Blob = await new Promise((resolve, reject) => {
		canvas.toBlob((b) => {
			if (b) resolve(b);
			else reject(new Error('Canvas toBlob failed'));
		}, 'image/png');
	});
	if (blob.size > maxBytes) {
		throw new CropSizeLimitError(blob.size, maxBytes);
	}
	return blob;
}

async function exportAnimatedImage(
	image: HTMLImageElement,
	displayDimensions: Size,
	cropDimensions: Size,
	cropOrigin: Point,
	rotationDeg: number,
	maxW: number,
	maxH: number,
	maxBytes: number,
	bytes: ArrayBuffer,
	format: AnimatedCropSourceFormat,
	signal: AbortSignal,
	onProgress: (progress: CropProgress) => void,
): Promise<Blob> {
	const scale = image.naturalWidth / displayDimensions.width;
	const cropNativeWidth = cropDimensions.width * scale;
	const cropNativeHeight = cropDimensions.height * scale;
	const {w: targetW, h: targetH} = containSize(cropNativeWidth, cropNativeHeight, maxW, maxH);
	const geom = computeCropGeometry({
		image,
		displayDimensions,
		cropDimensions,
		cropOrigin,
		maxDimensions: {width: targetW, height: targetH},
		rotationDeg,
	});
	const crop: CropParams = {
		x: Math.max(0, Math.floor(geom.sourceX)),
		y: Math.max(0, Math.floor(geom.sourceY)),
		width: Math.max(1, Math.floor(geom.sourceWidth)),
		height: Math.max(1, Math.floor(geom.sourceHeight)),
		rotation: toCropRotation(rotationDeg),
		outputWidth: Math.max(1, Math.floor(targetW)),
		outputHeight: Math.max(1, Math.floor(targetH)),
	};
	snapCropToImageBounds(crop, image);
	const result = await cropAnimatedImage({bytes, format, crop, sizeLimitBytes: maxBytes}, {signal, onProgress});
	const resultBlob = new Blob([new Uint8Array(result.bytes)], {type: result.mime});
	if (resultBlob.size === 0) {
		throw new Error('Empty animated image blob returned');
	}
	if (resultBlob.size > maxBytes) {
		throw new CropSizeLimitError(resultBlob.size, maxBytes);
	}
	return resultBlob;
}

function snapCropToImageBounds(options: CropParams, image: HTMLImageElement): void {
	const EPS = 2;
	const naturalWidth = image.naturalWidth;
	const naturalHeight = image.naturalHeight;
	if (Math.abs(options.x) <= EPS) {
		options.x = 0;
	}
	if (Math.abs(options.y) <= EPS) {
		options.y = 0;
	}
	const rightEdge = options.x + options.width;
	if (Math.abs(rightEdge - naturalWidth) <= EPS) {
		options.width = naturalWidth - options.x;
	}
	const bottomEdge = options.y + options.height;
	if (Math.abs(bottomEdge - naturalHeight) <= EPS) {
		options.height = naturalHeight - options.y;
	}
	if (options.x === 0 && Math.abs(options.width - naturalWidth) <= EPS) {
		options.width = naturalWidth;
	}
	if (options.y === 0 && Math.abs(options.height - naturalHeight) <= EPS) {
		options.height = naturalHeight;
	}
	if (Math.abs(options.outputWidth - naturalWidth) <= EPS) {
		options.outputWidth = naturalWidth;
	}
	if (Math.abs(options.outputHeight - naturalHeight) <= EPS) {
		options.outputHeight = naturalHeight;
	}
}

function getCropErrorMessage(
	i18n: I18n,
	error: unknown,
	fallbackMessage: string,
	sizeLimitBytes: number,
): string | null {
	if (error instanceof CropSizeLimitError) {
		return i18n._(CROPPED_IMAGE_IS_TOO_LARGE_PLEASE_CHOOSE_A_DESCRIPTOR, {
			maxSizeLabel: formatFileSize(i18n.locale, error.maxBytes),
		});
	}
	if (!(error instanceof CropPipelineError)) {
		return fallbackMessage;
	}
	switch (error.code) {
		case 'aborted':
			return null;
		case 'too_large':
			return i18n._(CROPPED_IMAGE_IS_TOO_LARGE_PLEASE_CHOOSE_A_DESCRIPTOR, {
				maxSizeLabel: formatFileSize(i18n.locale, sizeLimitBytes),
			});
		case 'budget_exceeded':
			return i18n._(ANIMATION_TOO_LONG_OR_TOO_LARGE_DESCRIPTOR);
		case 'decode_failed':
			return i18n._(IMAGE_COULDN_T_BE_READ_DESCRIPTOR);
		case 'animated_avif_unsupported':
			return i18n._(BROWSER_CAN_T_CROP_ANIMATED_AVIF_DESCRIPTOR, {avifFormatLabel: AVIF_FORMAT_LABEL});
		case 'wasm_unavailable':
			return i18n._(WEBASSEMBLY_TURNED_OFF_DESCRIPTOR);
		default:
			return fallbackMessage;
	}
}

function getCropProgressLabel(i18n: I18n, progress: CropProgress): string {
	switch (progress.phase) {
		case 'init':
			return i18n._(CROP_PROGRESS_PREPARING_DESCRIPTOR);
		case 'crop':
			return i18n._(CROP_PROGRESS_CROPPING_DESCRIPTOR, {
				percent: Math.round(Math.min(1, Math.max(0, progress.fraction)) * 100),
			});
		case 'compress':
			return i18n._(CROP_PROGRESS_COMPRESSING_DESCRIPTOR);
	}
}

function hasEditsFrom(zoomRatio: number, rotation: number, transform: Point, heightRatio: number): boolean {
	return zoomRatio !== 1 || rotation !== 0 || transform.x !== 0 || transform.y !== 0 || heightRatio !== 1;
}

function cropCoversDisplayedImage(displayDimensions: Size, cropDimensions: Size, zoomRatio: number): boolean {
	const EPS = 1;
	return (
		Math.abs(cropDimensions.width - displayDimensions.width * zoomRatio) <= EPS &&
		Math.abs(cropDimensions.height - displayDimensions.height * zoomRatio) <= EPS
	);
}

interface ImageCropModalProps {
	imageUrl: string;
	onCropComplete: (croppedImageBlob: Blob) => void;
	onSkip?: () => void;
	title: React.ReactNode;
	description: React.ReactNode;
	saveButtonLabel: React.ReactNode;
	errorMessage: string;
	aspectRatio: number;
	cropShape?: 'rect' | 'round';
	maxWidth: number;
	maxHeight: number;
	sizeLimitBytes: number;
	minHeightRatio?: number;
	maxHeightRatio?: number;
	canSkipOriginal?: (dimensions: Size) => boolean;
}

export const ImageCropModal: React.FC<ImageCropModalProps> = observer(
	({
		imageUrl,
		onCropComplete,
		onSkip,
		title,
		description,
		saveButtonLabel,
		errorMessage,
		aspectRatio,
		cropShape = 'rect',
		maxWidth,
		maxHeight,
		sizeLimitBytes,
		minHeightRatio,
		maxHeightRatio,
		canSkipOriginal,
	}) => {
		const {i18n} = useLingui();
		const popOwningModal = usePopOwningModal();
		const abortRef = useRef<AbortController | null>(null);
		const [progress, setProgress] = useState<CropProgress | null>(null);
		const imageRef = useRef<HTMLImageElement | null>(null);
		const cropperContainerRef = useRef<HTMLDivElement | null>(null);
		const transformRef = useRef<Point>({x: 0, y: 0});
		const [displayDimensions, setDisplayDimensions] = useState<Size | null>(null);
		const [cropDimensions, setCropDimensions] = useState<Size | null>(null);
		const [dragBoundaries, setDragBoundaries] = useState<DragBoundaries>({
			top: 0,
			bottom: 0,
			left: 0,
			right: 0,
		});
		const [zoomRatio, setZoomRatio] = useState(1);
		const [rotation, setRotation] = useState(0);
		const [hasEdits, setHasEdits] = useState(false);
		const [isProcessing, setIsProcessing] = useState(false);
		const [isDragging, setIsDragging] = useState(false);
		const [dragStart, setDragStart] = useState<Point>({x: 0, y: 0});
		const [loadError, setLoadError] = useState(false);
		const [sliderKey, setSliderKey] = useState(0);
		const [heightRatio, setHeightRatio] = useState(1);
		const [heightSliderKey, setHeightSliderKey] = useState(0);
		const [isSkipAllowed, setIsSkipAllowed] = useState(false);
		const isRound = cropShape === 'round';
		const MIN_ZOOM = 1;
		const MAX_ZOOM = 3;
		const effectiveMinHeightRatio = !isRound ? (minHeightRatio ?? 1) : 1;
		const effectiveMaxHeightRatio = !isRound ? (maxHeightRatio ?? 1) : 1;
		const heightSliderEnabled = !isRound && effectiveMinHeightRatio < effectiveMaxHeightRatio;
		const applyTransform = useCallback((x: number, y: number, rotationDeg: number) => {
			transformRef.current = {x, y};
			const img = imageRef.current;
			if (!img) return;
			img.style.transform = `translate3d(calc(-50% + ${x}px), calc(-50% + ${y}px), 0) rotate(${rotationDeg}deg)`;
		}, []);
		const recalculateLayout = useCallback(
			(nextHeightRatio: number, resetTransform: boolean) => {
				const img = imageRef.current;
				const container = cropperContainerRef.current;
				if (!img || !container) return;
				const containerRect = container.getBoundingClientRect();
				const containerWidth = containerRect.width;
				const containerHeight = containerRect.height;
				const naturalWidth = img.naturalWidth;
				const naturalHeight = img.naturalHeight;
				if (!naturalWidth || !naturalHeight || !containerWidth || !containerHeight) {
					return;
				}
				const scale = Math.min(containerWidth / naturalWidth, containerHeight / naturalHeight);
				const fittedWidth = naturalWidth * scale;
				const fittedHeight = naturalHeight * scale;
				const baseAspect = isRound ? 1 : aspectRatio;
				const clampedHeightRatio = heightSliderEnabled
					? clamp(nextHeightRatio, effectiveMinHeightRatio, effectiveMaxHeightRatio)
					: 1;
				const effectiveAspect = baseAspect / clampedHeightRatio;
				const cropRect = computeCropRect(fittedWidth, fittedHeight, effectiveAspect);
				const zoom = resetTransform ? 1 : zoomRatio;
				const rotationDeg = resetTransform ? 0 : rotation;
				const scaledWidth = fittedWidth * zoom;
				const scaledHeight = fittedHeight * zoom;
				const bounds = computeDragBoundaries(scaledWidth, scaledHeight, cropRect);
				const nextTransform = resetTransform ? {x: 0, y: 0} : transformRef.current;
				const clampedTransform = clampTransformToBounds(nextTransform.x, nextTransform.y, bounds);
				applyTransform(clampedTransform.x, clampedTransform.y, rotationDeg);
				setDisplayDimensions({width: fittedWidth, height: fittedHeight});
				setCropDimensions(cropRect);
				setDragBoundaries(bounds);
				setZoomRatio(zoom);
				setRotation(rotationDeg);
				setHeightRatio(clampedHeightRatio);
				setHasEdits(hasEditsFrom(zoom, rotationDeg, clampedTransform, clampedHeightRatio));
			},
			[
				applyTransform,
				aspectRatio,
				effectiveMaxHeightRatio,
				effectiveMinHeightRatio,
				heightSliderEnabled,
				isRound,
				rotation,
				zoomRatio,
			],
		);
		const handleImageLoad = useCallback(() => {
			const img = imageRef.current;
			if (img && onSkip) {
				setIsSkipAllowed(
					canSkipOriginal
						? canSkipOriginal({width: img.naturalWidth, height: img.naturalHeight})
						: img.naturalWidth > 0 && img.naturalHeight > 0,
				);
			} else {
				setIsSkipAllowed(false);
			}
			recalculateLayout(heightRatio, true);
		}, [canSkipOriginal, heightRatio, onSkip, recalculateLayout]);
		const handleMouseDown: React.MouseEventHandler<HTMLImageElement> = useCallback((event) => {
			if (event.button !== 0) return;
			event.preventDefault();
			const {x, y} = transformRef.current;
			setIsDragging(true);
			setDragStart({
				x: event.clientX - x,
				y: event.clientY - y,
			});
		}, []);
		const handleMouseMove = useCallback(
			(event: MouseEvent) => {
				if (!isDragging || !displayDimensions || !cropDimensions) return;
				const newX = event.clientX - dragStart.x;
				const newY = event.clientY - dragStart.y;
				const clamped = clampTransformToBounds(newX, newY, dragBoundaries);
				applyTransform(clamped.x, clamped.y, rotation);
				setHasEdits(hasEditsFrom(zoomRatio, rotation, clamped, heightRatio));
			},
			[
				applyTransform,
				cropDimensions,
				dragBoundaries,
				dragStart.x,
				dragStart.y,
				displayDimensions,
				heightRatio,
				isDragging,
				rotation,
				zoomRatio,
			],
		);
		const handleMouseUp = useCallback(() => {
			if (!isDragging) return;
			setIsDragging(false);
			const transform = transformRef.current;
			setHasEdits(hasEditsFrom(zoomRatio, rotation, transform, heightRatio));
		}, [heightRatio, isDragging, rotation, zoomRatio]);
		const handleZoomChange = useCallback(
			(ratio: number) => {
				if (!displayDimensions || !cropDimensions) return;
				const clampedZoom = clamp(ratio, MIN_ZOOM, MAX_ZOOM);
				const scaledWidth = displayDimensions.width * clampedZoom;
				const scaledHeight = displayDimensions.height * clampedZoom;
				const newBounds = computeDragBoundaries(scaledWidth, scaledHeight, cropDimensions);
				let {x, y} = transformRef.current;
				if (!inRange(x, newBounds.right, newBounds.left) || !inRange(y, newBounds.top, newBounds.bottom)) {
					const clamped = clampTransformToBounds(x, y, newBounds);
					x = clamped.x;
					y = clamped.y;
					applyTransform(x, y, rotation);
				}
				setZoomRatio(clampedZoom);
				setDragBoundaries(newBounds);
				setHasEdits(hasEditsFrom(clampedZoom, rotation, {x, y}, heightRatio));
			},
			[MIN_ZOOM, MAX_ZOOM, applyTransform, cropDimensions, displayDimensions, heightRatio, rotation],
		);
		const handleHeightRatioChange = useCallback(
			(value: number) => {
				if (!heightSliderEnabled) return;
				recalculateLayout(value, false);
			},
			[heightSliderEnabled, recalculateLayout],
		);
		const handleRotate = useCallback(() => {
			if (!displayDimensions || !cropDimensions) return;
			const nextRotation = (rotation + 90) % 360;
			const current = transformRef.current;
			const rotatedTransform = rotatePoint(current, 90);
			const scaledWidth = displayDimensions.width * zoomRatio;
			const scaledHeight = displayDimensions.height * zoomRatio;
			const newBounds = computeDragBoundaries(scaledWidth, scaledHeight, cropDimensions);
			const clamped = clampTransformToBounds(rotatedTransform.x, rotatedTransform.y, newBounds);
			applyTransform(clamped.x, clamped.y, nextRotation);
			setRotation(nextRotation);
			setDragBoundaries(newBounds);
			setHasEdits(hasEditsFrom(zoomRatio, nextRotation, clamped, heightRatio));
		}, [applyTransform, cropDimensions, displayDimensions, heightRatio, rotation, zoomRatio]);
		const handleReset = useCallback(() => {
			if (!displayDimensions || !cropDimensions) return;
			recalculateLayout(1, true);
			setSliderKey((k) => k + 1);
			setHeightSliderKey((k) => k + 1);
		}, [cropDimensions, displayDimensions, recalculateLayout]);
		const handleSave = useCallback(async () => {
			const img = imageRef.current;
			if (!img || !displayDimensions || !cropDimensions) return;
			if (
				!hasEdits &&
				isSkipAllowed &&
				onSkip &&
				cropCoversDisplayedImage(displayDimensions, cropDimensions, zoomRatio)
			) {
				onSkip();
				popOwningModal();
				return;
			}
			abortRef.current?.abort();
			const controller = new AbortController();
			abortRef.current = controller;
			const cropOrigin = transformRef.current;
			try {
				setIsProcessing(true);
				setProgress(null);
				const scaledDisplayDimensions: Size = {
					width: displayDimensions.width * zoomRatio,
					height: displayDimensions.height * zoomRatio,
				};
				const response = await fetch(imageUrl);
				if (controller.signal.aborted) return;
				if (!response.ok) {
					throw new Error('Failed to fetch image data');
				}
				const bytes = await response.arrayBuffer();
				if (controller.signal.aborted) return;
				const inspected = await inspectImageBytes(new Uint8Array(bytes));
				if (controller.signal.aborted) return;
				const outBlob =
					inspected.animated && isAnimatedCropSourceFormat(inspected.format)
						? await exportAnimatedImage(
								img,
								scaledDisplayDimensions,
								cropDimensions,
								cropOrigin,
								rotation,
								maxWidth,
								maxHeight,
								sizeLimitBytes,
								bytes,
								inspected.format,
								controller.signal,
								setProgress,
							)
						: await exportStaticImage(
								img,
								scaledDisplayDimensions,
								cropDimensions,
								cropOrigin,
								rotation,
								maxWidth,
								maxHeight,
								sizeLimitBytes,
							);
				if (controller.signal.aborted) return;
				onCropComplete(outBlob);
				popOwningModal();
			} catch (error) {
				if (controller.signal.aborted) return;
				const message = getCropErrorMessage(i18n, error, errorMessage, sizeLimitBytes);
				if (message == null) return;
				logger.error('Error cropping image:', error);
				showMessagingErrorModal({
					title: i18n._(COULDN_T_CROP_IMAGE_DESCRIPTOR),
					message,
					dataFlx: 'messaging.image-crop-modal.crop-failed.generic-error-modal',
				});
			} finally {
				if (abortRef.current === controller) {
					abortRef.current = null;
					setIsProcessing(false);
					setProgress(null);
				}
			}
		}, [
			cropDimensions,
			displayDimensions,
			errorMessage,
			hasEdits,
			i18n,
			imageUrl,
			isSkipAllowed,
			maxHeight,
			maxWidth,
			onCropComplete,
			onSkip,
			popOwningModal,
			rotation,
			sizeLimitBytes,
			zoomRatio,
		]);
		const handleSkip = useCallback(() => {
			if (!isSkipAllowed) return;
			if (onSkip) onSkip();
			popOwningModal();
		}, [isSkipAllowed, onSkip, popOwningModal]);
		const handleCancel = useCallback(() => {
			abortRef.current?.abort();
			popOwningModal();
		}, [popOwningModal]);
		useEffect(() => () => abortRef.current?.abort(), []);
		useEffect(() => {
			setIsSkipAllowed(false);
			setLoadError(false);
		}, [imageUrl]);
		useEffect(() => {
			const onMouseMove = (e: MouseEvent) => handleMouseMove(e);
			const onMouseUp = () => handleMouseUp();
			window.addEventListener('mousemove', onMouseMove);
			window.addEventListener('mouseup', onMouseUp);
			window.addEventListener('resize', handleImageLoad);
			return () => {
				window.removeEventListener('mousemove', onMouseMove);
				window.removeEventListener('mouseup', onMouseUp);
				window.removeEventListener('resize', handleImageLoad);
			};
		}, [handleImageLoad, handleMouseMove, handleMouseUp]);
		useEffect(() => {
			let isSliderDragging = false;
			const onSliderDragStart = () => {
				isSliderDragging = true;
			};
			const onSliderDragEnd = () => {
				setTimeout(() => {
					isSliderDragging = false;
				}, 50);
			};
			const onClickCapture = (e: MouseEvent) => {
				if (isSliderDragging) {
					e.stopPropagation();
				}
			};
			document.addEventListener('slider-drag-start', onSliderDragStart);
			document.addEventListener('slider-drag-end', onSliderDragEnd);
			window.addEventListener('click', onClickCapture, {capture: true});
			return () => {
				document.removeEventListener('slider-drag-start', onSliderDragStart);
				document.removeEventListener('slider-drag-end', onSliderDragEnd);
				window.removeEventListener('click', onClickCapture, {capture: true});
			};
		}, []);
		const imageStyle: React.CSSProperties = useMemo(() => {
			if (!displayDimensions) return {};
			const width = displayDimensions.width * zoomRatio;
			const height = displayDimensions.height * zoomRatio;
			return {
				width,
				height,
				minWidth: width,
				minHeight: height,
			};
		}, [displayDimensions, zoomRatio]);
		const exportDisabled = isProcessing || loadError;
		const progressLabel = isProcessing && progress != null ? getCropProgressLabel(i18n, progress) : null;
		return (
			<Modal.Root size="medium" onClose={handleCancel} data-flx="messaging.image-crop-modal.modal-root">
				<Modal.Header title={title} onClose={handleCancel} data-flx="messaging.image-crop-modal.modal-header" />
				<Modal.Content className={styles.content} data-flx="messaging.image-crop-modal.content">
					<div className={styles.description} data-flx="messaging.image-crop-modal.description">
						{description}
					</div>
					<div
						className={styles.cropperContainer}
						ref={cropperContainerRef}
						data-flx="messaging.image-crop-modal.cropper-container"
					>
						<img
							ref={imageRef}
							src={imageUrl}
							alt={i18n._(CROP_PREVIEW_DESCRIPTOR)}
							className={styles.image}
							style={{
								opacity: displayDimensions ? 1 : 0,
								transform: `translate3d(calc(-50% + ${transformRef.current.x}px), calc(-50% + ${
									transformRef.current.y
								}px), 0) rotate(${rotation}deg)`,
								...imageStyle,
							}}
							onLoad={handleImageLoad}
							onError={() => setLoadError(true)}
							onMouseDown={handleMouseDown}
							draggable={false}
							crossOrigin="anonymous"
							data-flx="messaging.image-crop-modal.image.mouse-down"
						/>
						{cropDimensions && !isRound && (
							<div
								className={styles.overlayRect}
								style={{
									width: cropDimensions.width,
									height: cropDimensions.height,
								}}
								aria-hidden
								data-flx="messaging.image-crop-modal.overlay-rect"
							/>
						)}
						{cropDimensions && isRound && (
							<div aria-hidden className={styles.roundOverlay} data-flx="messaging.image-crop-modal.round-overlay">
								<div
									className={styles.roundMask}
									style={{
										width: cropDimensions.width,
										height: cropDimensions.height,
									}}
									data-flx="messaging.image-crop-modal.round-mask"
								/>
							</div>
						)}
					</div>
					<div className={styles.controlsContainer} data-flx="messaging.image-crop-modal.controls-container">
						<div className={styles.sliderGroup} data-flx="messaging.image-crop-modal.slider-group">
							<div className={styles.sliderContainer} data-flx="messaging.image-crop-modal.slider-container">
								<div className={styles.sliderLabel} data-flx="messaging.image-crop-modal.slider-label">
									<Trans>Zoom</Trans>
								</div>
								<div
									className={styles.zoomSliderContainer}
									role="none"
									onMouseDown={(e) => {
										e.stopPropagation();
									}}
									data-flx="messaging.image-crop-modal.zoom-slider-container.stop-propagation"
								>
									<ImageSquareIcon
										size={12}
										weight="fill"
										className={styles.zoomIconSmall}
										data-flx="messaging.image-crop-modal.zoom-icon-small"
									/>
									<div className={styles.sliderWrapper} data-flx="messaging.image-crop-modal.slider-wrapper">
										<Slider
											key={sliderKey}
											value={zoomRatio}
											minValue={MIN_ZOOM}
											maxValue={MAX_ZOOM}
											factoryDefaultValue={1}
											step={Math.min(0.01, (MAX_ZOOM - MIN_ZOOM) / 200)}
											onValueChange={(z) => handleZoomChange(z)}
											asValueChanges={(z) => handleZoomChange(z)}
											defaultValue={1}
											data-flx="messaging.image-crop-modal.slider"
										/>
									</div>
									<ImageSquareIcon
										size={24}
										weight="fill"
										className={styles.zoomIconLarge}
										data-flx="messaging.image-crop-modal.zoom-icon-large"
									/>
								</div>
							</div>
							{heightSliderEnabled && (
								<div className={styles.sliderContainer} data-flx="messaging.image-crop-modal.slider-container--2">
									<div className={styles.sliderLabel} data-flx="messaging.image-crop-modal.slider-label--2">
										<Trans>Height</Trans>
									</div>
									<div
										className={styles.heightSliderContainer}
										role="none"
										onMouseDown={(e) => {
											e.stopPropagation();
										}}
										data-flx="messaging.image-crop-modal.height-slider-container.stop-propagation"
									>
										<div
											className={styles.heightIconShort}
											aria-hidden
											data-flx="messaging.image-crop-modal.height-icon-short"
										/>
										<div className={styles.sliderWrapper} data-flx="messaging.image-crop-modal.slider-wrapper--2">
											<Slider
												key={heightSliderKey}
												value={heightRatio}
												minValue={effectiveMinHeightRatio}
												maxValue={effectiveMaxHeightRatio}
												factoryDefaultValue={1}
												step={0.01}
												onValueChange={(v) => handleHeightRatioChange(v)}
												asValueChanges={(v) => handleHeightRatioChange(v)}
												defaultValue={1}
												data-flx="messaging.image-crop-modal.slider--2"
											/>
										</div>
										<div
											className={styles.heightIconTall}
											aria-hidden
											data-flx="messaging.image-crop-modal.height-icon-tall"
										/>
									</div>
								</div>
							)}
						</div>
						<FocusRing offset={-2} enabled={!isProcessing} data-flx="messaging.image-crop-modal.focus-ring">
							<button
								type="button"
								className={styles.rotateButton}
								onClick={handleRotate}
								disabled={isProcessing}
								aria-label={i18n._(ROTATE_CLOCKWISE_DESCRIPTOR)}
								data-flx="messaging.image-crop-modal.rotate-button"
							>
								<ArrowClockwiseIcon
									size={24}
									weight="regular"
									className={styles.rotateIcon}
									data-flx="messaging.image-crop-modal.rotate-icon"
								/>
							</button>
						</FocusRing>
					</div>
				</Modal.Content>
				<Modal.Footer data-flx="messaging.image-crop-modal.modal-footer">
					<Button
						variant="secondary"
						onClick={handleReset}
						disabled={isProcessing || !hasEdits}
						data-flx="messaging.image-crop-modal.button.reset"
					>
						<Trans>Reset</Trans>
					</Button>
					<Button variant="secondary" onClick={handleCancel} data-flx="messaging.image-crop-modal.button.cancel">
						<Trans>Cancel</Trans>
					</Button>
					{onSkip && isSkipAllowed && (
						<Button
							variant="secondary"
							onClick={handleSkip}
							disabled={isProcessing}
							data-flx="messaging.image-crop-modal.button.skip"
						>
							<Trans>Skip cropping</Trans>
						</Button>
					)}
					<Button
						onClick={handleSave}
						disabled={exportDisabled}
						submitting={isProcessing && progressLabel == null}
						data-flx="messaging.image-crop-modal.button.save"
					>
						{progressLabel ?? saveButtonLabel}
					</Button>
				</Modal.Footer>
			</Modal.Root>
		);
	},
);
