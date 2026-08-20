// SPDX-License-Identifier: AGPL-3.0-or-later

import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import '@app/features/voice/components/VoiceCallView.module.css';
import Accessibility from '@app/features/accessibility/state/Accessibility';
import Channels from '@app/features/channel/state/Channels';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import {FloatingPaneResizeHandles} from '@app/features/ui/floating_pane';
import PiP, {PIP_DEFAULT_WIDTH} from '@app/features/ui/state/PiP';
import {appZoomClientPoint, getAppZoomViewportSize} from '@app/features/ui/utils/AppZoomUtils';
import {canUseWindowFocusedHoverControls} from '@app/features/ui/utils/WindowFocusInteractionGuard';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import styles from '@app/features/voice/components/PiPOverlay.module.css';
import {PiPFooter} from '@app/features/voice/components/pip_overlay/PiPFooter';
import {PiPHeader} from '@app/features/voice/components/pip_overlay/PiPHeader';
import {PiPOverlayContextMenu} from '@app/features/voice/components/pip_overlay/PiPOverlayContextMenu';
import {
	type Corner,
	clamp,
	clampPiPWidth,
	computePiPResize,
	getCornerPositions,
	getDragBounds,
	getPiPHeight,
	getTitlebarHeight,
	INTERACTION_SPRING,
	type PiPOverlayInnerProps,
	pickCornerOnRelease,
	pipOverlayLogger,
	type ResizeEdge,
	type ResizeListeners,
	type ResizeState,
	SNAP_SPRING,
} from '@app/features/voice/components/pip_overlay/shared';
import {useFindTrackRef} from '@app/features/voice/components/pip_overlay/useFindTrackRef';
import {getStreamKey} from '@app/features/voice/components/StreamKeys';
import {useStreamSpectators} from '@app/features/voice/components/useStreamSpectators';
import {VoiceParticipantTile} from '@app/features/voice/components/VoiceParticipantTile';
import MediaEngine, {useMediaEngineVersion} from '@app/features/voice/engine/MediaEngineFacade';
import {VOICE_DISCONNECT_DESCRIPTOR} from '@app/features/voice/utils/VoiceMessageDescriptors';
import {ME} from '@fluxer/constants/src/AppConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {isTrackReference, type TrackReferenceOrPlaceholder} from '@livekit/components-react';
import {clsx} from 'clsx';
import type {AnimationPlaybackControls} from 'framer-motion';
import {animate, motion, useMotionValue} from 'framer-motion';
import type {Room} from 'livekit-client';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useLayoutEffect, useMemo, useRef, useState} from 'react';

const RESIZE_PICTURE_IN_PICTURE_DESCRIPTOR = msg({
	message: 'Resize picture-in-picture',
	comment: 'Aria label on the PiP resize handle.',
});
const PIP_DRAG_THRESHOLD_SQ = 9;

interface DragListeners {
	move: (event: PointerEvent) => void;
	up: (event: PointerEvent) => void;
}

interface DragState {
	pointerId: number;
	startX: number;
	startY: number;
	startPosX: number;
	startPosY: number;
	lastX: number;
	lastY: number;
	lastTimestamp: number;
	velocityX: number;
	velocityY: number;
	dragging: boolean;
}

export const PiPOverlayInner = observer(function PiPOverlayInner({content, room}: PiPOverlayInnerProps) {
	if (!room) {
		return (
			<PiPOverlayInnerBase
				content={content}
				room={null}
				trackRef={null}
				data-flx="voice.pip-overlay.pi-p-overlay-inner.pi-p-overlay-inner-base"
			/>
		);
	}
	return (
		<PiPOverlayInnerWithLiveKitTrack
			content={content}
			room={room}
			data-flx="voice.pip-overlay.pi-p-overlay-inner.pi-p-overlay-inner-with-live-kit-track"
		/>
	);
});

const PiPOverlayInnerWithLiveKitTrack = observer(function PiPOverlayInnerWithLiveKitTrack({
	content,
	room,
}: PiPOverlayInnerProps & {room: Room}) {
	const trackRef = useFindTrackRef(content, room);
	return (
		<PiPOverlayInnerBase
			content={content}
			room={room}
			trackRef={trackRef}
			data-flx="voice.pip-overlay.pi-p-overlay-inner.pi-p-overlay-inner-with-live-kit-track.pi-p-overlay-inner-base"
		/>
	);
});

interface PiPOverlayInnerBaseProps extends PiPOverlayInnerProps {
	trackRef: TrackReferenceOrPlaceholder | null;
}

const PiPOverlayInnerBase = observer(function PiPOverlayInnerBase({content, room, trackRef}: PiPOverlayInnerBaseProps) {
	const {i18n} = useLingui();
	useMediaEngineVersion();
	const corner = PiP.getEffectiveCorner();
	const isScreenShare = content.type === 'stream';
	const effectiveTrackRef = trackRef;
	const trackRefSummary = useMemo(() => {
		if (!effectiveTrackRef) return null;
		const summary = {
			participantIdentity: effectiveTrackRef.participant.identity,
			source: effectiveTrackRef.source,
			isTrackReference: isTrackReference(effectiveTrackRef),
		};
		if (!isTrackReference(effectiveTrackRef)) return summary;
		const publicationRecord = effectiveTrackRef.publication as unknown as Record<string, unknown>;
		return {
			...summary,
			trackSid: effectiveTrackRef.publication.trackSid,
			isMuted: effectiveTrackRef.publication.isMuted,
			isSubscribed: typeof publicationRecord.isSubscribed === 'boolean' ? publicationRecord.isSubscribed : null,
			hasTrack: Boolean(effectiveTrackRef.publication.track),
		};
	}, [effectiveTrackRef]);
	const channel = Channels.getChannel(content.channelId);
	const participantUser = Users.getUser(content.userId);
	const streamKey = useMemo(
		() => getStreamKey(content.guildId, content.channelId, content.connectionId),
		[content.guildId, content.channelId, content.connectionId],
	);
	const {viewerUsers} = useStreamSpectators(isScreenShare ? streamKey : '', content.userId);
	const displayName = useMemo(() => {
		if (!participantUser) return '';
		return NicknameUtils.getNickname(participantUser, content.guildId ?? null, content.channelId);
	}, [participantUser, content.guildId, content.channelId]);
	const disconnectLabel = i18n._(VOICE_DISCONNECT_DESCRIPTOR);
	const channelName = channel?.name ?? '';
	const [viewportSize, setViewportSize] = useState(getAppZoomViewportSize);
	const [pipWidth, setPipWidth] = useState(() => {
		const viewport = getAppZoomViewportSize();
		return clampPiPWidth(PiP.getWidth() || PIP_DEFAULT_WIDTH, viewport.width, viewport.height);
	});
	const [isResizing, setIsResizing] = useState(false);
	const [isDragging, setIsDragging] = useState(false);
	const pipWidthRef = useRef(pipWidth);
	const viewportSizeRef = useRef(viewportSize);
	const titlebarHeight = useMemo(() => getTitlebarHeight(), [viewportSize.height, viewportSize.width]);
	const titlebarHeightRef = useRef(titlebarHeight);
	const cornerRef = useRef(corner);
	const pipHeight = useMemo(() => getPiPHeight(pipWidth), [pipWidth]);
	const bounds = useMemo(
		() => getDragBounds(viewportSize.width, viewportSize.height, pipWidth, pipHeight, titlebarHeight),
		[viewportSize.width, viewportSize.height, pipHeight, pipWidth, titlebarHeight],
	);
	const cornerPositions = useMemo(
		() => getCornerPositions(viewportSize.width, viewportSize.height, pipWidth, pipHeight, titlebarHeight),
		[viewportSize.width, viewportSize.height, pipHeight, pipWidth, titlebarHeight],
	);
	const initialPosition = cornerPositions[corner];
	const x = useMotionValue(initialPosition.x);
	const y = useMotionValue(initialPosition.y);
	const widthMV = useMotionValue(pipWidth);
	const isDraggingRef = useRef(false);
	const isResizingRef = useRef(false);
	const animRef = useRef<{x?: AnimationPlaybackControls; y?: AnimationPlaybackControls}>({});
	const dragStateRef = useRef<DragState | null>(null);
	const dragListenersRef = useRef<DragListeners | null>(null);
	const pendingDragMoveRef = useRef<{pointerId: number; clientX: number; clientY: number; timeStamp: number} | null>(
		null,
	);
	const dragFrameRef = useRef<number | null>(null);
	const resizeStateRef = useRef<ResizeState | null>(null);
	const resizeListenersRef = useRef<ResizeListeners | null>(null);
	const pendingResizeMoveRef = useRef<{pointerId: number; clientX: number; clientY: number} | null>(null);
	const resizeFrameRef = useRef<number | null>(null);
	const viewportResizeFrameRef = useRef<number | null>(null);
	const stopSnapAnimations = useCallback(() => {
		animRef.current.x?.stop();
		animRef.current.y?.stop();
		animRef.current = {};
	}, []);
	const snapToCorner = useCallback(
		(targetCorner: Corner, opts?: {immediate?: boolean}) => {
			const next = cornerPositions[targetCorner];
			stopSnapAnimations();
			if (opts?.immediate || Accessibility.useReducedMotion) {
				x.set(next.x);
				y.set(next.y);
				return;
			}
			animRef.current.x = animate(x, next.x, SNAP_SPRING);
			animRef.current.y = animate(y, next.y, SNAP_SPRING);
		},
		[cornerPositions, stopSnapAnimations, x, y],
	);
	const snapToCornerForGeometry = useCallback(
		(targetCorner: Corner, width: number, viewport: {width: number; height: number}, opts?: {immediate?: boolean}) => {
			const nextHeight = getPiPHeight(width);
			const next = getCornerPositions(viewport.width, viewport.height, width, nextHeight, titlebarHeightRef.current)[
				targetCorner
			];
			stopSnapAnimations();
			if (opts?.immediate || Accessibility.useReducedMotion) {
				x.set(next.x);
				y.set(next.y);
				return;
			}
			animRef.current.x = animate(x, next.x, SNAP_SPRING);
			animRef.current.y = animate(y, next.y, SNAP_SPRING);
		},
		[stopSnapAnimations, x, y],
	);
	useLayoutEffect(() => {
		pipWidthRef.current = pipWidth;
		if (!isResizingRef.current) {
			widthMV.set(pipWidth);
		}
	}, [pipWidth, widthMV]);
	useLayoutEffect(() => {
		viewportSizeRef.current = viewportSize;
	}, [viewportSize]);
	useLayoutEffect(() => {
		titlebarHeightRef.current = titlebarHeight;
	}, [titlebarHeight]);
	useLayoutEffect(() => {
		cornerRef.current = corner;
	}, [corner]);
	const reconcileGeometry = useCallback(
		(options: {snapToCornerImmediate?: boolean} = {}) => {
			const {width: viewportWidth, height: viewportHeight} = getAppZoomViewportSize();
			const nextTitlebarHeight = getTitlebarHeight();
			const previousWidth = pipWidthRef.current;
			const clampedWidth = clampPiPWidth(previousWidth, viewportWidth, viewportHeight);
			const clampedHeight = getPiPHeight(clampedWidth);
			const nextBounds = getDragBounds(viewportWidth, viewportHeight, clampedWidth, clampedHeight, nextTitlebarHeight);
			pipWidthRef.current = clampedWidth;
			viewportSizeRef.current = {width: viewportWidth, height: viewportHeight};
			titlebarHeightRef.current = nextTitlebarHeight;
			if (!isResizingRef.current) {
				widthMV.set(clampedWidth);
			}
			if (!isDraggingRef.current && !isResizingRef.current) {
				if (options.snapToCornerImmediate) {
					const positions = getCornerPositions(
						viewportWidth,
						viewportHeight,
						clampedWidth,
						clampedHeight,
						nextTitlebarHeight,
					);
					const target = positions[cornerRef.current];
					x.set(target.x);
					y.set(target.y);
				} else {
					x.set(clamp(x.get(), nextBounds.minX, nextBounds.maxX));
					y.set(clamp(y.get(), nextBounds.minY, nextBounds.maxY));
				}
			}
			setViewportSize((previousViewportSize) => {
				if (previousViewportSize.width === viewportWidth && previousViewportSize.height === viewportHeight) {
					return previousViewportSize;
				}
				return {width: viewportWidth, height: viewportHeight};
			});
			setPipWidth((previousPipWidth) => {
				if (previousPipWidth === clampedWidth) return previousPipWidth;
				PiP.setWidth(clampedWidth);
				return clampedWidth;
			});
		},
		[widthMV, x, y],
	);
	useEffect(() => {
		const handleResize = () => {
			if (viewportResizeFrameRef.current !== null) return;
			viewportResizeFrameRef.current = requestAnimationFrame(() => {
				viewportResizeFrameRef.current = null;
				reconcileGeometry();
			});
		};
		window.addEventListener('resize', handleResize, {passive: true});
		const visualViewport = typeof window !== 'undefined' ? window.visualViewport : null;
		visualViewport?.addEventListener('resize', handleResize, {passive: true});
		visualViewport?.addEventListener('scroll', handleResize, {passive: true});
		let resizeObserver: ResizeObserver | null = null;
		if (typeof ResizeObserver !== 'undefined' && typeof document !== 'undefined') {
			resizeObserver = new ResizeObserver(handleResize);
			if (document.documentElement) resizeObserver.observe(document.documentElement);
			if (document.body) resizeObserver.observe(document.body);
		}
		const handleVisibility = () => {
			if (document.visibilityState === 'visible') handleResize();
		};
		document.addEventListener('visibilitychange', handleVisibility);
		handleResize();
		return () => {
			window.removeEventListener('resize', handleResize);
			visualViewport?.removeEventListener('resize', handleResize);
			visualViewport?.removeEventListener('scroll', handleResize);
			document.removeEventListener('visibilitychange', handleVisibility);
			resizeObserver?.disconnect();
			if (viewportResizeFrameRef.current !== null) {
				cancelAnimationFrame(viewportResizeFrameRef.current);
				viewportResizeFrameRef.current = null;
			}
		};
	}, [reconcileGeometry]);
	useLayoutEffect(() => {
		if (isDraggingRef.current || isResizingRef.current) return;
		snapToCorner(corner);
	}, [corner, snapToCorner]);
	useLayoutEffect(() => {
		if (isDraggingRef.current || isResizingRef.current) return;
		x.set(clamp(x.get(), bounds.minX, bounds.maxX));
		y.set(clamp(y.get(), bounds.minY, bounds.maxY));
	}, [bounds.maxX, bounds.maxY, bounds.minX, bounds.minY, x, y]);
	const cancelPendingResizeFrame = useCallback(() => {
		if (resizeFrameRef.current !== null) {
			cancelAnimationFrame(resizeFrameRef.current);
			resizeFrameRef.current = null;
		}
	}, []);
	const flushResizePointerMove = useCallback(() => {
		resizeFrameRef.current = null;
		const pendingMove = pendingResizeMoveRef.current;
		pendingResizeMoveRef.current = null;
		const state = resizeStateRef.current;
		if (!state || !pendingMove || state.pointerId !== pendingMove.pointerId) return;
		const viewport = viewportSizeRef.current;
		const pointer = appZoomClientPoint(pendingMove.clientX, pendingMove.clientY);
		const resize = computePiPResize(
			state,
			pointer.x,
			pointer.y,
			viewport.width,
			viewport.height,
			titlebarHeightRef.current,
		);
		widthMV.set(resize.width);
		pipWidthRef.current = resize.width;
		x.set(resize.offset.x);
		y.set(resize.offset.y);
	}, [widthMV, x, y]);
	const cleanupResizeListeners = useCallback(() => {
		const listeners = resizeListenersRef.current;
		if (!listeners) return;
		window.removeEventListener('pointermove', listeners.move);
		window.removeEventListener('pointerup', listeners.up);
		window.removeEventListener('pointercancel', listeners.up);
		resizeListenersRef.current = null;
		pendingResizeMoveRef.current = null;
		cancelPendingResizeFrame();
	}, [cancelPendingResizeFrame]);
	const handleResizePointerMove = useCallback(
		(event: PointerEvent) => {
			const state = resizeStateRef.current;
			if (!state || state.pointerId !== event.pointerId) return;
			event.preventDefault();
			pendingResizeMoveRef.current = {pointerId: event.pointerId, clientX: event.clientX, clientY: event.clientY};
			if (resizeFrameRef.current !== null) return;
			resizeFrameRef.current = requestAnimationFrame(flushResizePointerMove);
		},
		[flushResizePointerMove],
	);
	const handleResizePointerUp = useCallback(
		(event: PointerEvent) => {
			const state = resizeStateRef.current;
			if (!state || state.pointerId !== event.pointerId) return;
			event.preventDefault();
			if (resizeFrameRef.current !== null) {
				cancelPendingResizeFrame();
				flushResizePointerMove();
			}
			cleanupResizeListeners();
			resizeStateRef.current = null;
			isResizingRef.current = false;
			setIsResizing(false);
			setPipWidth(pipWidthRef.current);
			PiP.setWidth(pipWidthRef.current);
			snapToCornerForGeometry(cornerRef.current, pipWidthRef.current, viewportSizeRef.current);
		},
		[cancelPendingResizeFrame, cleanupResizeListeners, flushResizePointerMove, snapToCornerForGeometry],
	);
	const createResizePointerDownHandler = useCallback(
		(edge: ResizeEdge) => (event: React.PointerEvent<HTMLButtonElement>) => {
			if (event.button !== 0) return;
			if (!canUseWindowFocusedHoverControls()) return;
			event.preventDefault();
			event.stopPropagation();
			stopSnapAnimations();
			isResizingRef.current = true;
			setIsResizing(true);
			const pointer = appZoomClientPoint(event.clientX, event.clientY);
			resizeStateRef.current = {
				pointerId: event.pointerId,
				edge,
				startX: pointer.x,
				startY: pointer.y,
				startWidth: pipWidthRef.current,
				startPosX: x.get(),
				startPosY: y.get(),
			};
			const listeners: ResizeListeners = {
				move: handleResizePointerMove,
				up: handleResizePointerUp,
			};
			resizeListenersRef.current = listeners;
			window.addEventListener('pointermove', listeners.move);
			window.addEventListener('pointerup', listeners.up);
			window.addEventListener('pointercancel', listeners.up);
		},
		[handleResizePointerMove, handleResizePointerUp, stopSnapAnimations, x, y],
	);
	useEffect(() => cleanupResizeListeners, [cleanupResizeListeners]);
	const cancelPendingDragFrame = useCallback(() => {
		if (dragFrameRef.current !== null) {
			cancelAnimationFrame(dragFrameRef.current);
			dragFrameRef.current = null;
		}
	}, []);
	const flushDragPointerMove = useCallback(() => {
		dragFrameRef.current = null;
		const pendingMove = pendingDragMoveRef.current;
		pendingDragMoveRef.current = null;
		const state = dragStateRef.current;
		if (!state || !pendingMove || state.pointerId !== pendingMove.pointerId) return;
		const pointer = appZoomClientPoint(pendingMove.clientX, pendingMove.clientY);
		const deltaX = pointer.x - state.startX;
		const deltaY = pointer.y - state.startY;
		if (!state.dragging) {
			if (deltaX * deltaX + deltaY * deltaY <= PIP_DRAG_THRESHOLD_SQ) return;
			stopSnapAnimations();
			state.dragging = true;
			isDraggingRef.current = true;
			setIsDragging(true);
		}
		const viewport = viewportSizeRef.current;
		const width = pipWidthRef.current;
		const height = getPiPHeight(width);
		const currentBounds = getDragBounds(viewport.width, viewport.height, width, height, titlebarHeightRef.current);
		const nextX = clamp(state.startPosX + deltaX, currentBounds.minX, currentBounds.maxX);
		const nextY = clamp(state.startPosY + deltaY, currentBounds.minY, currentBounds.maxY);
		const elapsedMs = Math.max(1, pendingMove.timeStamp - state.lastTimestamp);
		state.velocityX = ((pointer.x - state.lastX) / elapsedMs) * 1000;
		state.velocityY = ((pointer.y - state.lastY) / elapsedMs) * 1000;
		state.lastX = pointer.x;
		state.lastY = pointer.y;
		state.lastTimestamp = pendingMove.timeStamp;
		x.set(nextX);
		y.set(nextY);
	}, [stopSnapAnimations, x, y]);
	const cleanupDragListeners = useCallback(() => {
		const listeners = dragListenersRef.current;
		if (!listeners) return;
		window.removeEventListener('pointermove', listeners.move);
		window.removeEventListener('pointerup', listeners.up);
		window.removeEventListener('pointercancel', listeners.up);
		dragListenersRef.current = null;
		pendingDragMoveRef.current = null;
		cancelPendingDragFrame();
	}, [cancelPendingDragFrame]);
	const handleDragPointerMove = useCallback(
		(event: PointerEvent) => {
			const state = dragStateRef.current;
			if (!state || state.pointerId !== event.pointerId) return;
			event.preventDefault();
			pendingDragMoveRef.current = {
				pointerId: event.pointerId,
				clientX: event.clientX,
				clientY: event.clientY,
				timeStamp: event.timeStamp,
			};
			if (dragFrameRef.current !== null) return;
			dragFrameRef.current = requestAnimationFrame(flushDragPointerMove);
		},
		[flushDragPointerMove],
	);
	const handleDragPointerUp = useCallback(
		(event: PointerEvent) => {
			const state = dragStateRef.current;
			if (!state || state.pointerId !== event.pointerId) return;
			if (dragFrameRef.current !== null) {
				cancelPendingDragFrame();
				flushDragPointerMove();
			}
			cleanupDragListeners();
			dragStateRef.current = null;
			if (!state.dragging) {
				return;
			}
			event.preventDefault();
			isDraggingRef.current = false;
			setIsDragging(false);
			const viewport = viewportSizeRef.current;
			const width = pipWidthRef.current;
			const height = getPiPHeight(width);
			const currentBounds = getDragBounds(viewport.width, viewport.height, width, height, titlebarHeightRef.current);
			const currentCorners = getCornerPositions(
				viewport.width,
				viewport.height,
				width,
				height,
				titlebarHeightRef.current,
			);
			const targetCorner = pickCornerOnRelease(
				x.get(),
				y.get(),
				state.velocityX,
				state.velocityY,
				currentCorners,
				currentBounds,
			);
			PiP.setCorner(targetCorner);
			snapToCornerForGeometry(targetCorner, width, viewport);
		},
		[cancelPendingDragFrame, cleanupDragListeners, flushDragPointerMove, snapToCornerForGeometry, x, y],
	);
	useEffect(() => cleanupDragListeners, [cleanupDragListeners]);
	const handleContainerPointerDown = useCallback(
		(event: React.PointerEvent<HTMLDivElement>) => {
			if (event.button !== 0) return;
			if (!canUseWindowFocusedHoverControls()) return;
			if (isResizingRef.current) return;
			const targetElement = event.target as HTMLElement | null;
			if (targetElement?.closest('button, [data-pip-no-drag="true"]')) return;
			cleanupDragListeners();
			const pointer = appZoomClientPoint(event.clientX, event.clientY);
			dragStateRef.current = {
				pointerId: event.pointerId,
				startX: pointer.x,
				startY: pointer.y,
				startPosX: x.get(),
				startPosY: y.get(),
				lastX: pointer.x,
				lastY: pointer.y,
				lastTimestamp: event.timeStamp,
				velocityX: 0,
				velocityY: 0,
				dragging: false,
			};
			const listeners: DragListeners = {
				move: handleDragPointerMove,
				up: handleDragPointerUp,
			};
			dragListenersRef.current = listeners;
			window.addEventListener('pointermove', listeners.move);
			window.addEventListener('pointerup', listeners.up);
			window.addEventListener('pointercancel', listeners.up);
		},
		[cleanupDragListeners, handleDragPointerMove, handleDragPointerUp, x, y],
	);
	const handleDisconnect = useCallback(async (e: React.MouseEvent) => {
		e.stopPropagation();
		await MediaEngine.disconnectFromVoiceChannel('user');
		PiP.close();
	}, []);
	const returnToCall = useCallback(() => {
		NavigationCommands.selectChannel(content.guildId ?? ME, content.channelId);
	}, [content.channelId, content.guildId]);
	const handleReturnToCall = useCallback(
		(event: React.MouseEvent<HTMLButtonElement>) => {
			event.stopPropagation();
			returnToCall();
		},
		[returnToCall],
	);
	const handleOverlayDoubleClick = useCallback(
		(event: React.MouseEvent<HTMLDivElement>) => {
			event.stopPropagation();
			if (isDraggingRef.current || isResizingRef.current) return;
			returnToCall();
		},
		[returnToCall],
	);
	const handleContextMenu = useCallback((event: React.MouseEvent<HTMLDivElement>) => {
		ContextMenuCommands.openFromEvent(event, () => (
			<PiPOverlayContextMenu data-flx="voice.pi-p-overlay.context-menu" />
		));
	}, []);
	useEffect(() => {
		pipOverlayLogger.debug('PiP overlay media state', {
			content,
			roomLocalParticipantIdentity: room?.localParticipant.identity ?? null,
			remoteParticipantCount: room?.remoteParticipants.size ?? 0,
			trackRef: trackRefSummary,
		});
	}, [content, room, trackRef, trackRefSummary]);
	return (
		<motion.div
			className={clsx(
				styles.container,
				isResizing && styles.containerResizing,
				(isDragging || isResizing) && styles.containerInteractionActive,
			)}
			style={{x, y, width: widthMV, scale: isDragging && !Accessibility.useReducedMotion ? 1.02 : 1}}
			onPointerDown={handleContainerPointerDown}
			onContextMenu={handleContextMenu}
			onDoubleClick={handleOverlayDoubleClick}
			transition={Accessibility.useReducedMotion ? {duration: 0} : INTERACTION_SPRING}
			data-flx="voice.pi-p-overlay.pi-p-overlay-inner.container"
		>
			{effectiveTrackRef && (
				<VoiceParticipantTile
					trackRef={effectiveTrackRef}
					guildId={content.guildId ?? undefined}
					channelId={content.channelId}
					showFocusIndicator={false}
					showParticipantMetadata={false}
					presentation="focus-main"
					data-flx="voice.pi-p-overlay.pi-p-overlay-inner.voice-participant-tile"
				/>
			)}
			<div className={styles.hoverOverlay} data-flx="voice.pi-p-overlay.pi-p-overlay-inner.hover-overlay">
				<PiPHeader
					channelName={channelName}
					onReturnToCall={handleReturnToCall}
					data-flx="voice.pip-overlay.pi-p-overlay-inner.pi-p-overlay-inner-base.pi-p-header"
				/>
				<PiPFooter
					displayName={displayName}
					isScreenShare={isScreenShare}
					viewerUsers={viewerUsers}
					disconnectLabel={disconnectLabel}
					onDisconnect={handleDisconnect}
					data-flx="voice.pip-overlay.pi-p-overlay-inner.pi-p-overlay-inner-base.pi-p-footer"
				/>
			</div>
			<FloatingPaneResizeHandles
				label={i18n._(RESIZE_PICTURE_IN_PICTURE_DESCRIPTOR)}
				createResizeHandler={createResizePointerDownHandler}
				data-flx="voice.pip-overlay.pi-p-overlay-inner.pi-p-overlay-inner-base.floating-pane-resize-handles"
			/>
		</motion.div>
	);
});
