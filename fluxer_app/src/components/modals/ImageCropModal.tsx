/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {Trans, useLingui} from '@lingui/react/macro';
import {ArrowClockwiseIcon, ImageSquareIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import styles from '~/components/modals/ImageCropModal.module.css';
import * as Modal from '~/components/modals/Modal';
import {Button} from '~/components/uikit/Button/Button';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {Slider} from '~/components/uikit/Slider';

interface CropGifWorkerMessage {
	type: number;
	result?: Uint8Array;
	error?: string;
}

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

export type GifBytes = Uint8Array;

interface GifCropOptions {
	x: number;
	y: number;
	width: number;
	height: number;
	imageRotation?: number;
	resizeWidth?: number | null;
	resizeHeight?: number | null;
}

async function cropGifWithWorker(gif: GifBytes, options: GifCropOptions): Promise<GifBytes> {
	const {x, y, width, height, imageRotation = 0, resizeWidth = null, resizeHeight = null} = options;

	return new Promise<GifBytes>((resolve, reject) => {
		const worker = new Worker(new URL('../../workers/gifCrop.worker.ts', import.meta.url), {
			type: 'module',
		});

		worker.addEventListener(
			'message',
			(event: MessageEvent<CropGifWorkerMessage>) => {
				const msg = event.data;

				if (msg.type === 1) {
					worker.terminate();
					if (msg.result) {
						resolve(msg.result);
					} else {
						reject(new Error('Empty result from worker'));
					}
				} else if (msg.type === 2) {
					worker.terminate();
					reject(new Error(msg.error || 'Unknown error from worker'));
				}
			},
			{once: true},
		);

		worker.addEventListener('error', (error) => {
			worker.terminate();
			reject(error);
		});

		const transferables: Array<Transferable> = [];
		if (gif.buffer) {
			transferables.push(gif.buffer);
		}

		worker.postMessage(
			{
				type: 0,
				gif,
				x,
				y,
				width,
				height,
				imageRotation,
				resizeWidth,
				resizeHeight,
			},
			transferables,
		);
	});
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
		throw new Error(`Image size ${(blob.size / 1024).toFixed(1)} KB exceeds max ${(maxBytes / 1024).toFixed(0)} KB`);
	}

	return blob;
}

async function exportAnimatedGif(
	image: HTMLImageElement,
	displayDimensions: Size,
	cropDimensions: Size,
	cropOrigin: Point,
	rotationDeg: number,
	maxW: number,
	maxH: number,
	maxBytes: number,
	src: string,
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

	const response = await fetch(src);
	if (!response.ok) {
		throw new Error('Failed to fetch GIF data');
	}
	const buffer = await response.arrayBuffer();
	const gifBytes = new Uint8Array(buffer);

	const cropOptions = {
		x: Math.max(0, Math.floor(geom.sourceX)),
		y: Math.max(0, Math.floor(geom.sourceY)),
		width: Math.max(1, Math.floor(geom.sourceWidth)),
		height: Math.max(1, Math.floor(geom.sourceHeight)),
		imageRotation: rotationDeg,
		resizeWidth: Math.floor(targetW),
		resizeHeight: Math.floor(targetH),
	};
	snapCropOptionsToImageBounds(cropOptions, image);

	const resultBytes = await cropGifWithWorker(gifBytes, cropOptions);
	const resultBlob = new Blob([new Uint8Array(resultBytes)], {type: 'image/gif'});

	if (resultBlob.size === 0) {
		throw new Error('Empty GIF blob returned');
	}

	if (resultBlob.size > maxBytes) {
		throw new Error(
			`GIF size ${(resultBlob.size / 1024).toFixed(1)} KB exceeds max ${(maxBytes / 1024).toFixed(0)} KB`,
		);
	}

	return resultBlob;
}

function snapCropOptionsToImageBounds(options: GifCropOptions, image: HTMLImageElement): void {
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

	if (options.resizeWidth != null && Math.abs(options.resizeWidth - naturalWidth) <= EPS) {
		options.resizeWidth = naturalWidth;
	}
	if (options.resizeHeight != null && Math.abs(options.resizeHeight - naturalHeight) <= EPS) {
		options.resizeHeight = naturalHeight;
	}
}

function hasEditsFrom(zoomRatio: number, rotation: number, transform: Point, heightRatio: number): boolean {
	return zoomRatio !== 1 || rotation !== 0 || transform.x !== 0 || transform.y !== 0 || heightRatio !== 1;
}

interface ImageCropModalProps {
	imageUrl: string;
	sourceMimeType: string;
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
}

export const ImageCropModal: React.FC<ImageCropModalProps> = observer(
	({
		imageUrl,
		sourceMimeType,
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
	}) => {
		const {t} = useLingui();
		const imageRef = React.useRef<HTMLImageElement | null>(null);
		const cropperContainerRef = React.useRef<HTMLDivElement | null>(null);
		const transformRef = React.useRef<Point>({x: 0, y: 0});

		const [displayDimensions, setDisplayDimensions] = React.useState<Size | null>(null);
		const [cropDimensions, setCropDimensions] = React.useState<Size | null>(null);
		const [dragBoundaries, setDragBoundaries] = React.useState<DragBoundaries>({
			top: 0,
			bottom: 0,
			left: 0,
			right: 0,
		});
		const [zoomRatio, setZoomRatio] = React.useState(1);
		const [rotation, setRotation] = React.useState(0);
		const [hasEdits, setHasEdits] = React.useState(false);
		const [isProcessing, setIsProcessing] = React.useState(false);
		const [isDragging, setIsDragging] = React.useState(false);
		const [dragStart, setDragStart] = React.useState<Point>({x: 0, y: 0});
		const [loadError, setLoadError] = React.useState(false);
		const [sliderKey, setSliderKey] = React.useState(0);

		const [heightRatio, setHeightRatio] = React.useState(1);
		const [heightSliderKey, setHeightSliderKey] = React.useState(0);

		const isRound = cropShape === 'round';
		const isGif = sourceMimeType.toLowerCase() === 'image/gif';

		const MIN_ZOOM = 1;
		const MAX_ZOOM = 3;

		const effectiveMinHeightRatio = !isRound ? (minHeightRatio ?? 1) : 1;
		const effectiveMaxHeightRatio = !isRound ? (maxHeightRatio ?? 1) : 1;
		const heightSliderEnabled = !isRound && effectiveMinHeightRatio < effectiveMaxHeightRatio;

		const applyTransform = React.useCallback((x: number, y: number, rotationDeg: number) => {
			transformRef.current = {x, y};
			const img = imageRef.current;
			if (!img) return;
			img.style.transform = `translate3d(calc(-50% + ${x}px), calc(-50% + ${y}px), 0) rotate(${rotationDeg}deg)`;
		}, []);

		const recalculateLayout = React.useCallback(
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

		const handleImageLoad = React.useCallback(() => {
			recalculateLayout(heightRatio, true);
		}, [heightRatio, recalculateLayout]);

		const handleMouseDown: React.MouseEventHandler<HTMLImageElement> = React.useCallback((event) => {
			event.preventDefault();
			const {x, y} = transformRef.current;
			setIsDragging(true);
			setDragStart({
				x: event.clientX - x,
				y: event.clientY - y,
			});
		}, []);

		const handleMouseMove = React.useCallback(
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

		const handleMouseUp = React.useCallback(() => {
			if (!isDragging) return;
			setIsDragging(false);
			const transform = transformRef.current;
			setHasEdits(hasEditsFrom(zoomRatio, rotation, transform, heightRatio));
		}, [heightRatio, isDragging, rotation, zoomRatio]);

		const handleZoomChange = React.useCallback(
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

		const handleHeightRatioChange = React.useCallback(
			(value: number) => {
				if (!heightSliderEnabled) return;
				recalculateLayout(value, false);
			},
			[heightSliderEnabled, recalculateLayout],
		);

		const handleRotate = React.useCallback(() => {
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

		const handleReset = React.useCallback(() => {
			if (!displayDimensions || !cropDimensions) return;

			recalculateLayout(1, true);
			setSliderKey((k) => k + 1);
			setHeightSliderKey((k) => k + 1);
		}, [cropDimensions, displayDimensions, recalculateLayout]);

		const handleSave = React.useCallback(async () => {
			const img = imageRef.current;
			if (!img || !displayDimensions || !cropDimensions) return;

			try {
				setIsProcessing(true);

				const scaledDisplayDimensions: Size = {
					width: displayDimensions.width * zoomRatio,
					height: displayDimensions.height * zoomRatio,
				};

				const outBlob = isGif
					? await exportAnimatedGif(
							img,
							scaledDisplayDimensions,
							cropDimensions,
							transformRef.current,
							rotation,
							maxWidth,
							maxHeight,
							sizeLimitBytes,
							imageUrl,
						)
					: await exportStaticImage(
							img,
							scaledDisplayDimensions,
							cropDimensions,
							transformRef.current,
							rotation,
							maxWidth,
							maxHeight,
							sizeLimitBytes,
						);

				onCropComplete(outBlob);
				ModalActionCreators.pop();
			} catch (error) {
				console.error('Error cropping image:', error);
				const message = error instanceof Error && error.message ? error.message : errorMessage;
				ToastActionCreators.createToast({
					type: 'error',
					children: message,
				});
			} finally {
				setIsProcessing(false);
			}
		}, [
			cropDimensions,
			displayDimensions,
			errorMessage,
			imageUrl,
			isGif,
			maxHeight,
			maxWidth,
			onCropComplete,
			rotation,
			sizeLimitBytes,
			zoomRatio,
		]);

		const handleSkip = React.useCallback(() => {
			if (onSkip) onSkip();
			ModalActionCreators.pop();
		}, [onSkip]);

		const handleCancel = React.useCallback(() => {
			ModalActionCreators.pop();
		}, []);

		React.useEffect(() => {
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

		React.useEffect(() => {
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

		const imageStyle: React.CSSProperties = React.useMemo(() => {
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

		return (
			<Modal.Root size="medium">
				<Modal.Header title={title} />
				<Modal.Content className={styles.content}>
					<div className={styles.description}>{description}</div>

					<div className={styles.cropperContainer} ref={cropperContainerRef}>
						<img
							ref={imageRef}
							src={imageUrl}
							alt="crop"
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
						/>

						{cropDimensions && !isRound && (
							<div
								className={styles.overlayRect}
								style={{
									width: cropDimensions.width,
									height: cropDimensions.height,
								}}
								aria-hidden
							/>
						)}

						{cropDimensions && isRound && (
							<div aria-hidden className={styles.roundOverlay}>
								<div
									className={styles.roundMask}
									style={{
										width: cropDimensions.width,
										height: cropDimensions.height,
									}}
								/>
							</div>
						)}
					</div>

					<div className={styles.controlsContainer}>
						<div className={styles.sliderGroup}>
							<div className={styles.sliderContainer}>
								<div className={styles.sliderLabel}>
									<Trans>Zoom</Trans>
								</div>
								<div
									className={styles.zoomSliderContainer}
									role="none"
									onMouseDown={(e) => {
										e.stopPropagation();
									}}
								>
									<ImageSquareIcon size={12} weight="fill" className={styles.zoomIconSmall} />
									<div className={styles.sliderWrapper}>
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
										/>
									</div>
									<ImageSquareIcon size={24} weight="fill" className={styles.zoomIconLarge} />
								</div>
							</div>

							{heightSliderEnabled && (
								<div className={styles.sliderContainer}>
									<div className={styles.sliderLabel}>
										<Trans>Height</Trans>
									</div>
									<div
										className={styles.heightSliderContainer}
										role="none"
										onMouseDown={(e) => {
											e.stopPropagation();
										}}
									>
										<div className={styles.heightIconShort} aria-hidden />
										<div className={styles.sliderWrapper}>
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
											/>
										</div>
										<div className={styles.heightIconTall} aria-hidden />
									</div>
								</div>
							)}
						</div>

						<FocusRing offset={-2} enabled={!isProcessing}>
							<button
								type="button"
								className={styles.rotateButton}
								onClick={handleRotate}
								disabled={isProcessing}
								title={t`Rotate clockwise`}
							>
								<ArrowClockwiseIcon size={24} weight="regular" className={styles.rotateIcon} />
							</button>
						</FocusRing>
					</div>
				</Modal.Content>
				<Modal.Footer>
					<div className={styles.footer}>
						<Button variant="secondary" onClick={handleReset} disabled={isProcessing || !hasEdits}>
							<Trans>Reset</Trans>
						</Button>
						<div className={styles.footerActions}>
							<Button variant="secondary" onClick={handleCancel} disabled={isProcessing}>
								<Trans>Cancel</Trans>
							</Button>
							{onSkip && (
								<Button variant="secondary" onClick={handleSkip} disabled={isProcessing}>
									<Trans>Skip Cropping</Trans>
								</Button>
							)}
							<Button onClick={handleSave} disabled={exportDisabled} submitting={isProcessing}>
								{saveButtonLabel}
							</Button>
						</div>
					</div>
				</Modal.Footer>
			</Modal.Root>
		);
	},
);
