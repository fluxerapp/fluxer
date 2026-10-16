// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/messaging/components/modals/MediaModal.module.css';
import {
	PanZoomSurface,
	type PanZoomSurfaceHandle,
} from '@app/features/messaging/components/modals/media_modal/pan_zoom/PanZoomSurface';
import type {PanZoomTransformSnapshot} from '@app/features/messaging/components/modals/media_modal/pan_zoom/usePanZoomSurface';
import type {ZoomState} from '@app/features/messaging/components/modals/media_modal/shared';
import type {ReactNode} from 'react';
import {forwardRef, memo} from 'react';

interface DesktopMediaViewerProps {
	children: ReactNode;
	onClose: () => void;
	onZoomStateChange: (state: ZoomState) => void;
	onTransformChange?: (snapshot: PanZoomTransformSnapshot) => void;
	resetKey?: unknown;
	zoomState: ZoomState;
}

export const DesktopMediaViewer = memo(
	forwardRef<PanZoomSurfaceHandle, DesktopMediaViewerProps>(function DesktopMediaViewer(
		{children, onClose, onZoomStateChange, onTransformChange, resetKey, zoomState}: DesktopMediaViewerProps,
		ref,
	) {
		return (
			<PanZoomSurface
				ref={ref}
				className={styles.desktopViewerContainer}
				contentClassName={styles.desktopViewerContent}
				contentRole="img"
				zoomState={zoomState}
				zoomedScale={2.5}
				maxScale={5}
				tapToToggleZoom
				doubleClickEnabled={false}
				resetKey={resetKey}
				onZoomStateChange={onZoomStateChange}
				onTransformChange={onTransformChange}
				onBackdropTap={onClose}
				data-flx="messaging.media-modal.desktop-media-viewer.pan-zoom-surface"
			>
				{children}
			</PanZoomSurface>
		);
	}),
);

interface MobileMediaViewerProps {
	children: ReactNode;
	onZoomStateChange: (state: ZoomState) => void;
	onTransformChange?: (snapshot: PanZoomTransformSnapshot) => void;
	resetKey?: unknown;
	zoomState: ZoomState;
}

export const MobileMediaViewer = memo(
	forwardRef<PanZoomSurfaceHandle, MobileMediaViewerProps>(function MobileMediaViewer(
		{children, onZoomStateChange, onTransformChange, resetKey, zoomState}: MobileMediaViewerProps,
		ref,
	) {
		return (
			<PanZoomSurface
				ref={ref}
				className={styles.mobileViewerContainer}
				contentClassName={styles.mobileViewerContent}
				zoomState={zoomState}
				zoomedScale={2.5}
				maxScale={5}
				tapToToggleZoom
				doubleClickEnabled={false}
				resetKey={resetKey}
				onZoomStateChange={onZoomStateChange}
				onTransformChange={onTransformChange}
				data-flx="messaging.media-modal.mobile-media-viewer.pan-zoom-surface"
			>
				{children}
			</PanZoomSurface>
		);
	}),
);
