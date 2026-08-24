// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ZoomState} from '@app/features/messaging/components/modals/media_modal/shared';
import {clsx} from 'clsx';
import {motion} from 'framer-motion';
import type {CSSProperties, HTMLAttributes, ReactNode} from 'react';
import {forwardRef, memo, useImperativeHandle} from 'react';
import styles from './PanZoomSurface.module.css';
import {type PanZoomTransformSnapshot, type UsePanZoomSurfaceOptions, usePanZoomSurface} from './usePanZoomSurface';

export interface PanZoomSurfaceHandle {
	reset: () => void;
	zoomIn: () => void;
	zoomOut: () => void;
	zoomTo: (state: ZoomState) => void;
	getSnapshot: () => PanZoomTransformSnapshot;
}

interface PanZoomSurfaceProps
	extends Omit<
		HTMLAttributes<HTMLDivElement>,
		'children' | 'onDoubleClick' | 'onPointerDown' | 'onPointerMove' | 'onPointerUp' | 'onWheel'
	> {
	children: ReactNode;
	contentClassName?: string;
	contentStyle?: CSSProperties;
	contentRole?: string;
	contentAriaLabel?: string;
	zoomState?: ZoomState;
	minScale?: number;
	maxScale?: number;
	zoomedScale?: number;
	disabled?: boolean;
	panDisabled?: boolean;
	wheelEnabled?: boolean;
	pinchEnabled?: boolean;
	doubleClickEnabled?: boolean;
	tapToToggleZoom?: boolean;
	resetKey?: unknown;
	onZoomStateChange?: (state: ZoomState) => void;
	onTransformChange?: (snapshot: PanZoomTransformSnapshot) => void;
	onTap?: () => void;
	onBackdropTap?: () => void;
}

function formatTransformLength(value: number | string): string {
	return typeof value === 'number' ? `${value.toFixed(3)}px` : value;
}

function formatTransformScale(value: number | string): string {
	return typeof value === 'number' ? value.toFixed(5) : value;
}

function buildPanZoomTransform({
	x = 0,
	y = 0,
	scale = 1,
}: {
	x?: number | string;
	y?: number | string;
	scale?: number | string;
}): string {
	const translate = `translate3d(${formatTransformLength(x)}, ${formatTransformLength(y)}, 0)`;
	return `${translate} scale(${formatTransformScale(scale)})`;
}

export const PanZoomSurface = memo(
	forwardRef<PanZoomSurfaceHandle, PanZoomSurfaceProps>(function PanZoomSurface(
		{
			children,
			className,
			contentClassName,
			contentStyle,
			contentRole,
			contentAriaLabel,
			style,
			zoomState,
			minScale,
			maxScale,
			zoomedScale,
			disabled,
			panDisabled,
			wheelEnabled,
			pinchEnabled,
			doubleClickEnabled,
			tapToToggleZoom,
			resetKey,
			onZoomStateChange,
			onTransformChange,
			onTap,
			onBackdropTap,
			...rest
		}: PanZoomSurfaceProps,
		ref,
	) {
		const controllerOptions: UsePanZoomSurfaceOptions = {
			zoomState,
			minScale,
			maxScale,
			zoomedScale,
			disabled,
			panDisabled,
			wheelEnabled,
			pinchEnabled,
			doubleClickEnabled,
			tapToToggleZoom,
			resetKey,
			onZoomStateChange,
			onTransformChange,
			onTap,
			onBackdropTap,
		};
		const controller = usePanZoomSurface(controllerOptions);
		useImperativeHandle(
			ref,
			() => ({
				reset: controller.reset,
				zoomIn: controller.zoomIn,
				zoomOut: controller.zoomOut,
				zoomTo: controller.zoomTo,
				getSnapshot: controller.getSnapshot,
			}),
			[controller.getSnapshot, controller.reset, controller.zoomIn, controller.zoomOut, controller.zoomTo],
		);
		return (
			<div
				data-flx="messaging.media-modal.pan-zoom.pan-zoom-surface.surface"
				{...rest}
				{...controller.viewportBindings}
				className={clsx(styles.surface, disabled && styles.surfaceDisabled, className)}
				style={{...style, cursor: controller.cursor}}
				data-zoom-state={controller.zoomState}
				data-dragging={controller.isDragging ? 'true' : undefined}
			>
				<motion.div
					data-flx="messaging.media-modal.pan-zoom.pan-zoom-surface.content"
					{...controller.contentBindings}
					className={clsx(styles.content, controller.isDragging && styles.contentDragging, contentClassName)}
					style={{
						...contentStyle,
						x: controller.x,
						y: controller.y,
						scale: controller.scale,
					}}
					transformTemplate={buildPanZoomTransform}
					role={contentRole}
					aria-label={contentAriaLabel}
				>
					{children}
				</motion.div>
			</div>
		);
	}),
);
