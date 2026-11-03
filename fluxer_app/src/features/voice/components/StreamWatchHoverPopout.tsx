// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {useMergeRefs} from '@app/features/app/hooks/useMergeRefs';
import {WATCH_STREAM_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import Permission from '@app/features/permissions/state/Permission';
import {usePortalHost} from '@app/features/ui/overlay/PortalHostContext';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {OVERLAY_STACK_BASE_Z_INDEX} from '@app/features/ui/state/OverlayStack';
import * as VoiceStreamWatchCommands from '@app/features/voice/commands/VoiceStreamWatchCommands';
import {parseStreamKey} from '@app/features/voice/components/StreamKeys';
import {StreamWatchHoverCard} from '@app/features/voice/components/StreamWatchHoverCard';
import {useStreamPreview} from '@app/features/voice/components/useStreamPreview';
import {useStreamWatchState} from '@app/features/voice/components/useStreamWatchState';
import MediaEngine, {useMediaEngineVersion} from '@app/features/voice/engine/MediaEngineFacade';
import {usePendingVoiceConnection} from '@app/features/voice/hooks/usePendingVoiceConnection';
import {canViewStreamPreview} from '@app/features/voice/utils/StreamPreviewPermissionUtils';
import {buildVoiceParticipantIdentity} from '@app/features/voice/utils/VoiceParticipantIdentity';
import {WATCHING_STREAM_DESCRIPTOR} from '@app/features/voice/utils/VoiceSharedMessageDescriptors';
import type {Placement} from '@floating-ui/react';
import {
	autoUpdate,
	FloatingPortal,
	flip,
	offset,
	safePolygon,
	shift,
	useFloating,
	useFocus,
	useHover,
	useInteractions,
} from '@floating-ui/react';
import {ME} from '@fluxer/constants/src/AppConstants';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import type {MotionStyle, Transition} from 'framer-motion';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type {FocusEvent, HTMLAttributes, MouseEvent, ReactElement, Ref, SyntheticEvent} from 'react';
import {Children, cloneElement, useCallback, useEffect, useMemo, useState} from 'react';

const YOU_RE_STREAMING_DESCRIPTOR = msg({
	message: "You're streaming!",
	comment: 'Status text on the screen-share hover popout shown to the local streamer. Lightly playful tone.',
});

interface StreamWatchHoverPopoutProps extends HTMLAttributes<HTMLElement> {
	streamKey: string;
	guildId: string | null;
	channelId: string | null;
	enabled: boolean;
	children: ReactElement<HTMLAttributes<HTMLElement> & {ref?: Ref<HTMLElement>}>;
}

const FLOATING_INITIAL = {opacity: 0, scale: 0.98};
const FLOATING_INITIAL_REDUCED = {opacity: 1, scale: 1};
const FLOATING_ANIMATE = {opacity: 1, scale: 1};
const FLOATING_EXIT = {opacity: 0, scale: 0.98};
const FLOATING_EXIT_REDUCED = {opacity: 1, scale: 1};
const FLOATING_TRANSITION: Transition = {
	opacity: {duration: 0.1},
	scale: {type: 'spring', damping: 25, stiffness: 500},
};
const FLOATING_TRANSITION_REDUCED: Transition = {duration: 0};
const FLOATING_PLACEMENT: Placement = 'right';
const FLOATING_Z_INDEX = OVERLAY_STACK_BASE_Z_INDEX - 2;
export const StreamWatchHoverPopout = observer(function StreamWatchHoverPopout({
	streamKey,
	guildId,
	channelId,
	enabled,
	children,
	...rest
}: StreamWatchHoverPopoutProps) {
	const {i18n} = useLingui();
	const portalRoot = usePortalHost();
	const isMobileLayout = MobileLayout.isMobileLayout();
	const isPopoutEnabled = useMemo(() => enabled && !isMobileLayout, [enabled, isMobileLayout]);
	const [isOpen, setIsOpen] = useState(false);
	const floatingMiddleware = useMemo(() => [offset(16), flip(), shift({padding: 8})], []);
	const floatingOptions = useMemo(
		() => ({
			open: isOpen,
			onOpenChange: setIsOpen,
			placement: FLOATING_PLACEMENT,
			middleware: floatingMiddleware,
			whileElementsMounted: autoUpdate,
		}),
		[floatingMiddleware, isOpen, setIsOpen],
	);
	const {x, y, refs, strategy, context} = useFloating(floatingOptions);
	useEffect(() => {
		if (!isPopoutEnabled && isOpen) {
			setIsOpen(false);
		}
	}, [isPopoutEnabled, isOpen]);
	const hoverDelay = useMemo(() => ({open: 200, close: 150}), []);
	const hoverSafePolygon = useMemo(() => safePolygon({buffer: 4, requireIntent: false}), []);
	const hoverOptions = useMemo(
		() => ({delay: hoverDelay, handleClose: hoverSafePolygon}),
		[hoverDelay, hoverSafePolygon],
	);
	const hover = useHover(context, hoverOptions);
	const focus = useFocus(context);
	const interactions = useMemo(() => [hover, focus], [focus, hover]);
	const {getReferenceProps, getFloatingProps} = useInteractions(interactions);
	const canFetchStreamPreview = canViewStreamPreview({
		guildId,
		channelId,
		hasConnectPermission: () =>
			Permission.can(Permissions.CONNECT, {guildId: guildId ?? undefined, channelId: channelId ?? undefined}),
	});
	const isPreviewActive = useMemo(
		() => isOpen && isPopoutEnabled && canFetchStreamPreview,
		[isOpen, isPopoutEnabled, canFetchStreamPreview],
	);
	const {previewUrl, isPreviewLoading} = useStreamPreview(isPreviewActive, streamKey);
	useMediaEngineVersion();
	const streamWatchStateArgs = useMemo(
		() => ({
			streamKey,
			guildId,
			channelId,
		}),
		[streamKey, guildId, channelId],
	);
	const {isWatching, isPendingJoin, canWatch, startWatching} = useStreamWatchState(streamWatchStateArgs);
	const streamWatchTarget = useMemo(() => {
		const parsed = parseStreamKey(streamKey);
		const connectionId = parsed?.connectionId;
		if (!connectionId) return null;
		const userId = MediaEngine.connectionVoiceStates[connectionId]?.user_id;
		if (!userId) return null;
		return {userId, connectionId};
	}, [streamKey, MediaEngine.connectionVoiceStates]);
	const handleWatchConnected = useCallback(() => {
		if (streamWatchTarget) {
			VoiceStreamWatchCommands.applyStreamWatchFocus({
				participantIdentity: buildVoiceParticipantIdentity(streamWatchTarget.userId, streamWatchTarget.connectionId),
				guildId,
				channelId,
			});
			return;
		}
		if (channelId) {
			NavigationCommands.selectChannel(guildId ?? ME, channelId);
		}
	}, [streamWatchTarget, guildId, channelId]);
	const {markPending: markWatchNavigationPending} = usePendingVoiceConnection({
		guildId,
		channelId,
		onConnected: handleWatchConnected,
	});
	const isOwnLocalStream = useMemo(() => {
		const localConnectionId = MediaEngine.connectionId;
		if (!localConnectionId || !streamKey) return false;
		const parsed = parseStreamKey(streamKey);
		return parsed?.connectionId === localConnectionId;
	}, [streamKey, MediaEngine.connectionId]);
	const child = Children.only(children);
	const referenceRefs = useMemo(() => [refs.setReference, child.props.ref], [refs.setReference, child.props.ref]);
	const mergedRef = useMergeRefs(referenceRefs);
	const watchLabel = useMemo(() => {
		if (isOwnLocalStream) return i18n._(YOU_RE_STREAMING_DESCRIPTOR);
		if (isWatching) return i18n._(WATCHING_STREAM_DESCRIPTOR);
		return i18n._(WATCH_STREAM_DESCRIPTOR);
	}, [isOwnLocalStream, isWatching, i18n.locale]);
	const watchDisabled = useMemo(
		() => isOwnLocalStream || !canWatch || isPendingJoin || isWatching,
		[isOwnLocalStream, canWatch, isPendingJoin, isWatching],
	);
	const fallbackProps = useMemo(() => ({...rest}), [rest]);
	const {
		onMouseEnter: onRestMouseEnter,
		onMouseLeave: onRestMouseLeave,
		onFocus: onRestFocus,
		onBlur: onRestBlur,
		onContextMenu: onRestContextMenu,
		onClick: onRestClick,
		...restProps
	} = rest;
	const {
		onMouseEnter: onChildMouseEnter,
		onMouseLeave: onChildMouseLeave,
		onFocus: onChildFocus,
		onBlur: onChildBlur,
		onContextMenu: onChildContextMenu,
		onClick: onChildClick,
	} = child.props;
	const stopPropagation = useCallback((event: SyntheticEvent) => {
		event.stopPropagation();
	}, []);
	const handleReferenceMouseEnter = useCallback(
		(event: MouseEvent<HTMLElement>) => {
			onChildMouseEnter?.(event);
			onRestMouseEnter?.(event);
		},
		[onChildMouseEnter, onRestMouseEnter],
	);
	const handleReferenceMouseLeave = useCallback(
		(event: MouseEvent<HTMLElement>) => {
			onChildMouseLeave?.(event);
			onRestMouseLeave?.(event);
		},
		[onChildMouseLeave, onRestMouseLeave],
	);
	const handleReferenceFocus = useCallback(
		(event: FocusEvent<HTMLElement>) => {
			onChildFocus?.(event);
			onRestFocus?.(event);
		},
		[onChildFocus, onRestFocus],
	);
	const handleReferenceBlur = useCallback(
		(event: FocusEvent<HTMLElement>) => {
			onChildBlur?.(event);
			onRestBlur?.(event);
		},
		[onChildBlur, onRestBlur],
	);
	const handleReferenceContextMenu = useCallback(
		(event: MouseEvent<HTMLElement>) => {
			onChildContextMenu?.(event);
			onRestContextMenu?.(event);
		},
		[onChildContextMenu, onRestContextMenu],
	);
	const handleReferenceClick = useCallback(
		(event: MouseEvent<HTMLElement>) => {
			onChildClick?.(event);
			onRestClick?.(event);
		},
		[onChildClick, onRestClick],
	);
	const referenceHandlers = useMemo(
		() => ({
			onMouseEnter: handleReferenceMouseEnter,
			onMouseLeave: handleReferenceMouseLeave,
			onFocus: handleReferenceFocus,
			onBlur: handleReferenceBlur,
			onContextMenu: handleReferenceContextMenu,
			onClick: handleReferenceClick,
		}),
		[
			handleReferenceMouseEnter,
			handleReferenceMouseLeave,
			handleReferenceFocus,
			handleReferenceBlur,
			handleReferenceContextMenu,
			handleReferenceClick,
		],
	);
	const referenceProps = useMemo(
		() =>
			getReferenceProps({
				...restProps,
				...referenceHandlers,
				ref: mergedRef,
				...(isOpen && {'data-popout-open': 'true'}),
			}),
		[getReferenceProps, isOpen, mergedRef, referenceHandlers, restProps],
	);
	const floatingProps = useMemo(
		() =>
			getFloatingProps({
				ref: refs.setFloating,
				onMouseDown: stopPropagation,
				onTouchStart: stopPropagation,
				onClick: stopPropagation,
			}),
		[getFloatingProps, refs.setFloating, stopPropagation],
	);
	const floatingStyles = useMemo(
		(): MotionStyle => ({
			position: strategy,
			left: x ?? 0,
			top: y ?? 0,
			zIndex: FLOATING_Z_INDEX,
			visibility: x === null || y === null ? 'hidden' : 'visible',
			pointerEvents: 'auto',
		}),
		[strategy, x, y],
	);
	const streamParticipantIdentity = useMemo(() => {
		const parsed = parseStreamKey(streamKey);
		if (!parsed) return null;
		const {connectionId} = parsed;
		if (!connectionId) return null;
		const participants = MediaEngine.participants;
		for (const participantIdentity in participants) {
			const participant = participants[participantIdentity];
			if (!participant) continue;
			if (participant.connectionId === connectionId) return participant.identity;
		}
		const voiceState = MediaEngine.connectionVoiceStates[connectionId];
		if (!voiceState?.user_id) return null;
		return buildVoiceParticipantIdentity(voiceState.user_id, connectionId);
	}, [streamKey, MediaEngine.participants, MediaEngine.connectionVoiceStates]);
	const handleWatch = useCallback(
		(event: SyntheticEvent) => {
			event.stopPropagation();
			if (watchDisabled) return;
			if (!streamWatchTarget) return;
			VoiceStreamWatchCommands.openAndWatchStream(
				{
					streamKey,
					guildId,
					channelId,
					userId: streamWatchTarget.userId,
					connectionId: streamWatchTarget.connectionId,
				},
				{startWatching, markPending: markWatchNavigationPending},
			);
		},
		[startWatching, watchDisabled, streamWatchTarget, streamKey, guildId, channelId, markWatchNavigationPending],
	);
	const handlePreviewClick = useCallback(
		(event: MouseEvent<Element>) => {
			event.stopPropagation();
			if (isWatching && streamParticipantIdentity) {
				VoiceStreamWatchCommands.applyStreamWatchFocus({
					participantIdentity: streamParticipantIdentity,
					guildId,
					channelId,
				});
			} else if (!watchDisabled) {
				handleWatch(event);
			}
		},
		[isWatching, streamParticipantIdentity, watchDisabled, handleWatch, guildId, channelId],
	);
	if (!isPopoutEnabled) {
		return cloneElement(child, fallbackProps);
	}
	return (
		<>
			{cloneElement(child, referenceProps)}
			<FloatingPortal root={portalRoot ?? undefined} data-flx="voice.stream-watch-hover-popout.floating-portal">
				<AnimatePresence data-flx="voice.stream-watch-hover-popout.animate-presence">
					{isOpen && (
						<motion.div
							data-flx="voice.stream-watch-hover-popout.div"
							{...floatingProps}
							style={floatingStyles}
							initial={Accessibility.useReducedMotion ? FLOATING_INITIAL_REDUCED : FLOATING_INITIAL}
							animate={FLOATING_ANIMATE}
							exit={Accessibility.useReducedMotion ? FLOATING_EXIT_REDUCED : FLOATING_EXIT}
							transition={Accessibility.useReducedMotion ? FLOATING_TRANSITION_REDUCED : FLOATING_TRANSITION}
						>
							<StreamWatchHoverCard
								variant="list"
								previewUrl={previewUrl}
								isPreviewLoading={isPreviewLoading}
								watchLabel={watchLabel}
								watchDisabled={watchDisabled}
								isWatching={isWatching}
								isSubmitting={isPendingJoin}
								onWatch={handleWatch}
								onPreviewClick={handlePreviewClick}
								showProtip={!isWatching}
								data-flx="voice.stream-watch-hover-popout.stream-watch-hover-card"
							/>
						</motion.div>
					)}
				</AnimatePresence>
			</FloatingPortal>
		</>
	);
});
