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

import {useLingui} from '@lingui/react/macro';
import {
	isTrackReference,
	TrackRefContext,
	type TrackReferenceOrPlaceholder,
	useIsSpeaking,
	useParticipantTile,
	VideoTrack,
} from '@livekit/components-react';
import {
	DesktopIcon,
	DeviceMobileIcon,
	DotsThreeIcon,
	EyeIcon,
	MicrophoneSlashIcon,
	PauseIcon,
	ProjectorScreenIcon,
	SpeakerSlashIcon,
	VideoCameraSlashIcon,
} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {type Participant, type RemoteTrackPublication, Track} from 'livekit-client';
import {autorun} from 'mobx';
import {observer} from 'mobx-react-lite';
import React, {useCallback, useEffect, useLayoutEffect, useMemo, useRef, useState} from 'react';
import * as ContextMenuActionCreators from '~/actions/ContextMenuActionCreators';
import * as SoundActionCreators from '~/actions/SoundActionCreators';
import * as VoiceCallLayoutActionCreators from '~/actions/VoiceCallLayoutActionCreators';
import {DEFAULT_ACCENT_COLOR} from '~/Constants';
import {VoiceParticipantBottomSheet} from '~/components/bottomsheets/VoiceParticipantBottomSheet';
import {LongPressable} from '~/components/LongPressable';
import {Avatar} from '~/components/uikit/Avatar';
import {Button} from '~/components/uikit/Button/Button';
import {VoiceParticipantContextMenu} from '~/components/uikit/ContextMenu/VoiceParticipantContextMenu';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {Tooltip} from '~/components/uikit/Tooltip';
import {Endpoints} from '~/Endpoints';
import HttpClient from '~/lib/HttpClient';
import type {UserRecord} from '~/records/UserRecord';
import CallMediaPrefsStore from '~/stores/CallMediaPrefsStore';
import ContextMenuStore from '~/stores/ContextMenuStore';
import LocalVoiceStateStore from '~/stores/LocalVoiceStateStore';
import UserStore from '~/stores/UserStore';
import VoiceCallLayoutStore from '~/stores/VoiceCallLayoutStore';
import MediaEngineStore from '~/stores/voice/MediaEngineFacade';
import type {VoiceState} from '~/stores/voice/VoiceStateManager';
import WindowStore from '~/stores/WindowStore';
import {isMobileExperienceEnabled} from '~/utils/mobileExperience';
import * as NicknameUtils from '~/utils/NicknameUtils';
import {SoundType} from '~/utils/SoundUtils';
import {getPlaceholderAvatarColor} from './getPlaceholderAvatarColor';
import voiceCallStyles from './VoiceCallView.module.css';
import styles from './VoiceParticipantTile.module.css';

const AVATAR_BACKGROUND_DIM_AMOUNT = 0.12;

interface RGBComponents {
	r: number;
	g: number;
	b: number;
	a?: number;
}

const clampChannel = (value: number) => Math.max(0, Math.min(255, value));

const toRGBComponents = (color: string): RGBComponents | null => {
	const normalized = color.trim();

	const hexMatch = normalized.match(/^#([a-f0-9]{3}|[a-f0-9]{6}|[a-f0-9]{8})$/i);
	if (hexMatch) {
		const hex = hexMatch[1];
		const expand = (pair: string) => (pair.length === 1 ? `${pair}${pair}` : pair);

		if (hex.length === 3) {
			const r = expand(hex[0]);
			const g = expand(hex[1]);
			const b = expand(hex[2]);
			return {
				r: parseInt(r, 16),
				g: parseInt(g, 16),
				b: parseInt(b, 16),
			};
		}

		if (hex.length === 6) {
			return {
				r: parseInt(hex.slice(0, 2), 16),
				g: parseInt(hex.slice(2, 4), 16),
				b: parseInt(hex.slice(4, 6), 16),
			};
		}

		if (hex.length === 8) {
			return {
				r: parseInt(hex.slice(0, 2), 16),
				g: parseInt(hex.slice(2, 4), 16),
				b: parseInt(hex.slice(4, 6), 16),
				a: parseInt(hex.slice(6, 8), 16) / 255,
			};
		}
	}

	const rgbMatch = normalized.match(
		/^rgba?\(\s*(\d{1,3})\s*,\s*(\d{1,3})\s*,\s*(\d{1,3})(?:\s*,\s*(\d*\.?\d+))?\s*\)$/i,
	);

	if (rgbMatch) {
		const [, rawR, rawG, rawB, rawA] = rgbMatch;
		const components: RGBComponents = {
			r: clampChannel(Number(rawR)),
			g: clampChannel(Number(rawG)),
			b: clampChannel(Number(rawB)),
		};

		if (rawA != null) {
			components.a = Math.max(0, Math.min(1, Number(rawA)));
		}

		return components;
	}

	return null;
};

const formatRGB = ({r, g, b, a}: RGBComponents): string =>
	a != null ? `rgba(${r}, ${g}, ${b}, ${a})` : `rgb(${r}, ${g}, ${b})`;

const dimColor = (color: string): string => {
	const rgb = toRGBComponents(color);
	if (!rgb) return color;

	const factor = Math.max(0, Math.min(1, 1 - AVATAR_BACKGROUND_DIM_AMOUNT));
	const dimmed: RGBComponents = {
		r: clampChannel(Math.round(rgb.r * factor)),
		g: clampChannel(Math.round(rgb.g * factor)),
		b: clampChannel(Math.round(rgb.b * factor)),
	};

	if (rgb.a != null) {
		dimmed.a = rgb.a;
	}

	return formatRGB(dimmed);
};

interface VoiceParticipantTileProps {
	trackRef?: TrackReferenceOrPlaceholder;
	guildId?: string;
	channelId?: string;
	onClick?: (participantIdentity: string) => void;
	isPinned?: boolean;
	showFocusIndicator?: boolean;
	allowAutoSubscribe?: boolean;
}

interface VoiceParticipantTileInnerProps {
	trackRef: TrackReferenceOrPlaceholder;
	elementProps: React.HTMLAttributes<HTMLElement>;
	guildId?: string;
	channelId?: string;
	onClick?: (participantIdentity: string) => void;
	isPinned?: boolean;
	showFocusIndicator?: boolean;
	allowAutoSubscribe: boolean;
}

interface ParsedIdentity {
	userId: string;
	connectionId: string;
}

function parseIdentity(identity: string): ParsedIdentity {
	const match = identity.match(/^user_(\d+)_(.+)$/);
	return {userId: match?.[1] ?? '', connectionId: match?.[2] ?? ''};
}

function getStreamKey(guildId: string | undefined, channelId: string | undefined, connectionId: string) {
	if (channelId && guildId) return `${guildId}:${channelId}:${connectionId}`;
	if (channelId) return `dm:${channelId}:${connectionId}`;
	return `stream:${connectionId}`;
}

function isVideoSource(source: Track.Source | undefined) {
	return source === Track.Source.Camera || source === Track.Source.ScreenShare;
}

function getSourceDataAttr(source: Track.Source | undefined) {
	switch (source) {
		case Track.Source.ScreenShare:
			return 'screen_share';
		case Track.Source.Camera:
			return 'camera';
		default:
			return 'other';
	}
}

function useEffectiveTrackRef(explicit?: TrackReferenceOrPlaceholder) {
	const ctx = React.useContext(TrackRefContext as React.Context<TrackReferenceOrPlaceholder | undefined>);
	return (explicit ?? ctx) as TrackReferenceOrPlaceholder | undefined;
}

function useIntersection<T extends Element>(enabled: boolean) {
	const ref = useRef<T | null>(null);
	const [isIntersecting, setIsIntersecting] = useState(false);

	useEffect(() => {
		if (typeof IntersectionObserver === 'undefined') return;
		if (!enabled) return;

		const el = ref.current;
		if (!el) return;

		const observer = new IntersectionObserver((entries) => setIsIntersecting(entries.some((e) => e.isIntersecting)), {
			threshold: [0, 0.1],
		});

		observer.observe(el);
		return () => observer.disconnect();
	}, [enabled]);

	return {ref, isIntersecting};
}

function useTileContextMenuActive(tileElRef: React.RefObject<HTMLElement | null>) {
	const [open, setOpen] = useState(false);

	useEffect(() => {
		const disposer = autorun(() => {
			const cm = ContextMenuStore.contextMenu;
			const target = cm?.target?.target as Node | undefined;
			const el = tileElRef.current;
			setOpen(Boolean(cm && target && el && el.contains(target)));
		});
		return () => disposer();
	}, [tileElRef]);

	return open;
}

function useAvatarSize(tileElRef: React.RefObject<HTMLElement | null>, min = 80, max = 192) {
	const [avatarSize, setAvatarSize] = useState(min);

	useLayoutEffect(() => {
		if (typeof ResizeObserver === 'undefined') return;

		const el = tileElRef.current;
		if (!el) return;

		const compute = () => {
			const rect = el.getBoundingClientRect();
			const base = Math.min(rect.width, rect.height);
			const next = Math.round(Math.max(min, Math.min(base * 0.32, max)));
			setAvatarSize((prev) => (prev !== next ? next : prev));
		};

		compute();
		const ro = new ResizeObserver(compute);
		ro.observe(el);
		return () => ro.disconnect();
	}, [tileElRef, min, max]);

	return avatarSize;
}

function useAutoVideoSubscription(opts: {
	enabled: boolean;
	trackRef: TrackReferenceOrPlaceholder;
	isIntersecting: boolean;
	videoLocallyDisabled: boolean;
	isLocalParticipant: boolean;
	isScreenShare: boolean;
}) {
	const {enabled, trackRef, isIntersecting, videoLocallyDisabled, isLocalParticipant, isScreenShare} = opts;

	useEffect(() => {
		if (!enabled) return;
		if (isLocalParticipant) return;
		if (isScreenShare) return;
		if (!isTrackReference(trackRef)) return;

		const pub = trackRef.publication as RemoteTrackPublication | undefined;
		if (!pub || typeof pub.setSubscribed !== 'function') return;

		const shouldSubscribe = isIntersecting && !videoLocallyDisabled;

		try {
			pub.setSubscribed(shouldSubscribe);
		} catch (err) {
			console.error('[VoiceParticipantTile] setSubscribed failed', err);
		}
	}, [enabled, trackRef, isIntersecting, videoLocallyDisabled, isLocalParticipant, isScreenShare]);
}

const previewInflight = new Map<string, Promise<string | null>>();

function useScreensharePreview(enabled: boolean, streamKey: string) {
	const [previewUrl, setPreviewUrl] = useState<string | null>(null);
	const [loading, setLoading] = useState(false);

	const revoke = useCallback((url: string | null) => {
		if (url) URL.revokeObjectURL(url);
	}, []);

	const fetchPreview = useCallback(async () => {
		if (!enabled) return;

		const existing = previewInflight.get(streamKey);
		if (existing) {
			const url = await existing;
			setPreviewUrl((prev) => {
				if (prev !== url) revoke(prev);
				return url;
			});
			return;
		}

		const p = (async () => {
			setLoading(true);
			try {
				const response = await HttpClient.get<ArrayBuffer>({
					url: Endpoints.STREAM_PREVIEW(streamKey),
					binary: true,
					skipParsing: true,
				});

				if (!response.ok || !response.body) return null;

				const contentType = response.headers['content-type'] || 'image/jpeg';
				const blob = new Blob([response.body], {type: contentType});
				return URL.createObjectURL(blob);
			} catch (err) {
				console.error('[VoiceParticipantTile] preview fetch failed', err);
				return null;
			} finally {
				setLoading(false);
			}
		})();

		previewInflight.set(streamKey, p);

		const url = await p.finally(() => previewInflight.delete(streamKey));
		setPreviewUrl((prev) => {
			if (prev !== url) revoke(prev);
			return url;
		});
	}, [enabled, streamKey, revoke]);

	useEffect(() => {
		return () => {
			setPreviewUrl((prev) => {
				revoke(prev);
				return null;
			});
		};
	}, [revoke]);

	return {previewUrl, isPreviewLoading: loading, fetchPreview};
}

function useScreenshareWatchSubscription(opts: {
	isScreenShare: boolean;
	trackRef: TrackReferenceOrPlaceholder;
	userWantsToWatch: boolean;
	videoLocallyDisabled: boolean;
	isWindowFocused: boolean;
}) {
	const {isScreenShare, trackRef, userWantsToWatch, videoLocallyDisabled, isWindowFocused} = opts;

	useEffect(() => {
		if (!isScreenShare) return;
		if (!userWantsToWatch) return;
		if (videoLocallyDisabled) return;
		if (!isTrackReference(trackRef)) return;

		const pub = trackRef.publication as RemoteTrackPublication | undefined;
		if (!pub || typeof pub.setSubscribed !== 'function' || typeof pub.setEnabled !== 'function') return;

		if (!pub.isSubscribed) {
			try {
				pub.setSubscribed(true);
			} catch (err) {
				console.error('[Screenshare] setSubscribed(true) failed', err);
			}
		}

		try {
			pub.setEnabled(isWindowFocused);
		} catch (err) {
			console.error('[Screenshare] setEnabled failed', err);
		}
	}, [isScreenShare, trackRef, userWantsToWatch, videoLocallyDisabled, isWindowFocused]);
}

function useWindowFocus() {
	const [isFocused, setIsFocused] = useState(() => WindowStore.isFocused());

	useEffect(() => {
		const disposer = autorun(() => {
			setIsFocused(WindowStore.isFocused());
		});
		return () => disposer();
	}, []);

	return isFocused;
}

function useViewerUsers(isScreenShare: boolean, streamKey: string, ownerUserId: string) {
	const viewerIds = useMemo(() => {
		if (!isScreenShare) return [];

		const allStates = MediaEngineStore.getAllVoiceStates();
		const ids: Array<string> = [];

		Object.values(allStates).forEach((guildStates) => {
			Object.values(guildStates).forEach((channelStates) => {
				Object.values(channelStates).forEach((vs: VoiceState) => {
					if (vs.viewer_stream_key === streamKey && vs.user_id && vs.user_id !== ownerUserId) {
						ids.push(vs.user_id);
					}
				});
			});
		});

		return Array.from(new Set(ids));
	}, [isScreenShare, streamKey, ownerUserId]);

	const viewerUsers = useMemo(
		() => viewerIds.map((id) => UserStore.getUser(id)).filter((u): u is UserRecord => Boolean(u)),
		[viewerIds],
	);

	return {viewerIds, viewerUsers};
}

export const VoiceParticipantTile = observer((props: VoiceParticipantTileProps) => {
	const {trackRef, guildId, channelId, onClick, isPinned, showFocusIndicator, allowAutoSubscribe = true} = props;

	const effectiveTrackRef = useEffectiveTrackRef(trackRef);
	const {elementProps} = useParticipantTile({
		trackRef: effectiveTrackRef ?? undefined,
		htmlProps: {},
	});

	if (!effectiveTrackRef) return null;

	return (
		<VoiceParticipantTileInner
			trackRef={effectiveTrackRef}
			elementProps={elementProps}
			guildId={guildId}
			channelId={channelId}
			onClick={onClick}
			isPinned={isPinned}
			showFocusIndicator={showFocusIndicator}
			allowAutoSubscribe={allowAutoSubscribe}
		/>
	);
});

const VoiceParticipantTileInner = observer(function VoiceParticipantTileInner({
	trackRef,
	elementProps,
	guildId,
	channelId,
	onClick,
	isPinned,
	showFocusIndicator,
	allowAutoSubscribe,
}: VoiceParticipantTileInnerProps) {
	const {t} = useLingui();
	const participant = trackRef.participant;
	const identity = participant.identity;
	const {userId, connectionId} = useMemo(() => parseIdentity(identity), [identity]);

	const participantUser = UserStore.getUser(userId);
	const currentUser = UserStore.getCurrentUser();
	const isCurrentUser = currentUser?.id === participantUser?.id;

	const isSpeaking = useIsSpeaking(participant);

	const voiceState = MediaEngineStore.getVoiceStateByConnectionId(connectionId);
	const connectionParticipant = MediaEngineStore.getParticipantByUserIdAndConnectionId(userId, connectionId);

	const isGuildMuted = voiceState?.mute ?? false;
	const isSelfMuted =
		voiceState?.self_mute ?? (connectionParticipant ? !connectionParticipant.isMicrophoneEnabled : false);
	const isSelfDeafened = voiceState?.self_deaf ?? false;

	const isActuallySpeaking = isSpeaking && !isSelfMuted && !isGuildMuted;

	const isMobileExperience = isMobileExperienceEnabled();
	const [bottomSheetOpen, setBottomSheetOpen] = useState(false);

	const isLocalParticipant = Boolean((participant as Participant)?.isLocal);
	const isWindowFocused = useWindowFocus();

	const sourceAttr = getSourceDataAttr(trackRef.source);
	const isScreenShare = trackRef.source === Track.Source.ScreenShare;
	const isOwnScreenShare = isScreenShare && isLocalParticipant;

	const callId = MediaEngineStore.connectionId ?? '';
	const streamKey = useMemo(() => getStreamKey(guildId, channelId, connectionId), [guildId, channelId, connectionId]);

	const videoLocallyDisabled = useMemo(() => {
		if (!callId) return false;
		if (!isTrackReference(trackRef)) return false;
		if (!isVideoSource(trackRef.source)) return false;
		return CallMediaPrefsStore.isVideoDisabled(callId, identity);
	}, [callId, trackRef, identity]);

	const publication = isTrackReference(trackRef)
		? (trackRef.publication as RemoteTrackPublication | undefined)
		: undefined;

	const hasVideo = useMemo(() => {
		if (!isTrackReference(trackRef)) return false;
		const pub = trackRef.publication;
		return Boolean(pub?.track) && !pub?.isMuted && !videoLocallyDisabled;
	}, [trackRef, videoLocallyDisabled]);

	const isSubscribed = Boolean(publication?.isSubscribed);

	const {ref: tileRef, isIntersecting} = useIntersection<HTMLDivElement>(allowAutoSubscribe);
	useAutoVideoSubscription({
		enabled: allowAutoSubscribe,
		trackRef,
		isIntersecting,
		videoLocallyDisabled,
		isLocalParticipant,
		isScreenShare,
	});

	const [userWantsToWatch, setUserWantsToWatch] = useState(false);

	const startWatching = useCallback(() => {
		setUserWantsToWatch(true);
		LocalVoiceStateStore.updateViewerStreamKey(streamKey);
		MediaEngineStore.syncLocalVoiceStateWithServer({viewer_stream_key: streamKey});
	}, [streamKey]);

	const stopWatching = useCallback(() => {
		setUserWantsToWatch(false);
		if (LocalVoiceStateStore.getViewerStreamKey()) {
			LocalVoiceStateStore.updateViewerStreamKey(null);
			MediaEngineStore.syncLocalVoiceStateWithServer({viewer_stream_key: null});
		}
	}, []);

	useScreenshareWatchSubscription({
		isScreenShare,
		trackRef,
		userWantsToWatch,
		videoLocallyDisabled,
		isWindowFocused,
	});

	useEffect(() => {
		if (!isScreenShare || videoLocallyDisabled) stopWatching();

		return () => {
			if (LocalVoiceStateStore.getViewerStreamKey() === streamKey) stopWatching();
		};
	}, [isScreenShare, videoLocallyDisabled, streamKey, stopWatching]);

	const [previewPopoverOpen, setPreviewPopoverOpen] = useState(false);
	const previewEnabled = isScreenShare && !isSubscribed && !videoLocallyDisabled;

	const {previewUrl, isPreviewLoading, fetchPreview} = useScreensharePreview(previewEnabled, streamKey);

	useEffect(() => {
		if (!previewEnabled) return;
		void fetchPreview();
	}, [previewEnabled, fetchPreview]);

	const {viewerIds, viewerUsers} = useViewerUsers(isScreenShare, streamKey, userId);

	const prevViewerCountRef = useRef(0);
	useEffect(() => {
		if (!isScreenShare) return;

		const prev = prevViewerCountRef.current;
		const next = viewerIds.length;

		if (next > prev) SoundActionCreators.playSound(SoundType.ViewerJoin);
		else if (next < prev) SoundActionCreators.playSound(SoundType.ViewerLeave);

		prevViewerCountRef.current = next;
	}, [isScreenShare, viewerIds.length]);

	const avatarSize = useAvatarSize(tileRef);
	const placeholderColor = useMemo(
		() => getPlaceholderAvatarColor(participantUser, DEFAULT_ACCENT_COLOR),
		[participantUser],
	);

	const placeholderBackgroundColor = useMemo(() => dimColor(placeholderColor), [placeholderColor]);

	const placeholderStyle = useMemo<React.CSSProperties>(
		() => ({backgroundColor: placeholderBackgroundColor}),
		[placeholderBackgroundColor],
	);

	const tileContextMenuOpen = useTileContextMenuActive(tileRef);

	const hasMultipleConnections = useMemo(() => {
		if (!guildId || !isCurrentUser) return false;

		const allStates = MediaEngineStore.getAllVoiceStates();
		let count = 0;

		Object.entries(allStates).forEach(([g, guildData]) => {
			if (g !== guildId) return;
			Object.values(guildData).forEach((channelData) => {
				Object.values(channelData).forEach((vs) => {
					if (vs.user_id === participantUser?.id) count += 1;
				});
			});
		});

		return count > 1;
	}, [guildId, participantUser?.id, isCurrentUser]);

	const participantDisplayName = useMemo(() => {
		return (
			(participantUser ? NicknameUtils.getNickname(participantUser, guildId, channelId) : participant.name) ||
			participantUser?.username ||
			'User'
		);
	}, [participantUser, guildId, channelId, participant.name]);

	const handleContextMenu = useCallback(
		(event: React.MouseEvent | MouseEvent) => {
			if (!participantUser) return;

			ContextMenuActionCreators.openFromEvent(event, ({onClose}) => (
				<VoiceParticipantContextMenu
					user={participantUser}
					participantName={participantDisplayName}
					onClose={onClose}
					guildId={guildId}
					connectionId={connectionId}
					isGroupedItem={hasMultipleConnections && isCurrentUser}
				/>
			));
		},
		[participantUser, participantDisplayName, guildId, connectionId, hasMultipleConnections, isCurrentUser],
	);

	const isFocused = VoiceCallLayoutStore.pinnedParticipantIdentity === identity;

	const handleTileClick = useCallback(() => {
		if (isScreenShare) return;

		const wasFocused = VoiceCallLayoutStore.pinnedParticipantIdentity === identity;
		if (wasFocused) {
			VoiceCallLayoutActionCreators.setPinnedParticipant(null);
			VoiceCallLayoutActionCreators.setLayoutMode('grid');
		} else {
			VoiceCallLayoutActionCreators.setLayoutMode('focus');
			VoiceCallLayoutActionCreators.setPinnedParticipant(identity);
			onClick?.(identity);
		}
		VoiceCallLayoutActionCreators.markUserOverride();
	}, [identity, isScreenShare, onClick]);

	const handleTileKeyDown = useCallback(
		(event: React.KeyboardEvent<HTMLDivElement>) => {
			switch (event.key) {
				case 'Enter':
				case ' ':
					event.preventDefault();
					event.stopPropagation();
					handleTileClick();
					break;
				default:
					break;
			}
		},
		[handleTileClick],
	);

	const handleMenuButtonKeyDown = useCallback(
		(event: React.KeyboardEvent<HTMLDivElement>) => {
			switch (event.key) {
				case 'Enter':
				case ' ': {
					event.preventDefault();
					event.stopPropagation();
					const node = event.currentTarget as HTMLElement | null;
					if (!node) return;

					const rect = node.getBoundingClientRect();
					const x = rect.left + rect.width / 2 + (window.scrollX || window.pageXOffset);
					const y = rect.top + rect.height / 2 + (window.scrollY || window.pageYOffset);
					const syntheticEvent = new MouseEvent('contextmenu', {
						clientX: rect.left + rect.width / 2,
						clientY: rect.top + rect.height / 2,
						screenX: x,
						screenY: y,
						bubbles: true,
						cancelable: true,
					});
					handleContextMenu(syntheticEvent);
					break;
				}
				default:
					break;
			}
		},
		[handleContextMenu],
	);

	const handleWatch = useCallback(
		(e?: React.SyntheticEvent) => {
			e?.stopPropagation();
			startWatching();
			VoiceCallLayoutActionCreators.setLayoutMode('focus');
			VoiceCallLayoutActionCreators.setPinnedParticipant(identity);
		},
		[identity, startWatching],
	);

	const handleMouseEnter = useCallback(() => {
		if (!previewEnabled) return;
		setPreviewPopoverOpen(true);
		void fetchPreview();
	}, [previewEnabled, fetchPreview]);

	const handleMouseLeave = useCallback(() => setPreviewPopoverOpen(false), []);

	const mediaNode = useMemo(() => {
		if (isTrackReference(trackRef) && hasVideo) {
			return <VideoTrack trackRef={trackRef} manageSubscription={false} />;
		}

		return (
			<div style={placeholderStyle} className={voiceCallStyles.lkParticipantPlaceholder}>
				{participantUser && (
					<div className={isActuallySpeaking ? styles.avatarRingSpeaking : styles.avatarRing}>
						<Avatar user={participantUser} size={avatarSize} className={styles.avatarFlexShrink} guildId={guildId} />
					</div>
				)}
			</div>
		);
	}, [trackRef, hasVideo, placeholderStyle, participantUser, isActuallySpeaking, avatarSize, guildId]);

	return (
		<>
			<FocusRing offset={-2}>
				<LongPressable
					ref={tileRef as React.Ref<HTMLDivElement>}
					{...elementProps}
					className={clsx(
						voiceCallStyles.lkParticipantTile,
						elementProps.className,
						isPinned && voiceCallStyles.pinnedParticipant,
						!isScreenShare && styles.cursorPointer,
						tileContextMenuOpen && voiceCallStyles.tileContextMenuActive,
					)}
					data-speaking={isActuallySpeaking}
					data-video-muted={!hasVideo}
					data-source={sourceAttr}
					onContextMenu={handleContextMenu}
					onClick={handleTileClick}
					onKeyDown={handleTileKeyDown}
					onLongPress={() => {
						if (isMobileExperience && participantUser) setBottomSheetOpen(true);
					}}
					onMouseEnter={handleMouseEnter}
					onMouseLeave={handleMouseLeave}
					role="button"
					tabIndex={0}
				>
					{mediaNode}

					{isScreenShare &&
						isTrackReference(trackRef) &&
						!userWantsToWatch &&
						!videoLocallyDisabled &&
						!isOwnScreenShare && (
							<div className={styles.watchStreamOverlay}>
								<Button
									variant="secondary"
									compact
									fitContent
									leftIcon={<ProjectorScreenIcon className={styles.watchStreamButtonIcon} />}
									onClick={handleWatch}
								>
									{t`Watch Stream`}
								</Button>
								<div className={styles.liveBadge}>LIVE</div>
							</div>
						)}

					{isOwnScreenShare && (
						<div className={styles.selfStreamOverlay}>
							{isWindowFocused ? (
								<div className={styles.selfStreamPreviewActive}>
									<div className={styles.liveBadge}>LIVE</div>
								</div>
							) : (
								<div className={styles.selfStreamPreviewPaused}>
									<PauseIcon weight="fill" className={styles.pausedIcon} />
									<span className={styles.pausedText}>{t`Preview paused to save resources`}</span>
									<span className={styles.pausedSubtext}>{t`Your stream is still being broadcast`}</span>
								</div>
							)}
						</div>
					)}

					{isScreenShare && previewEnabled && previewPopoverOpen && !isOwnScreenShare && (
						<div className={styles.previewPopover}>
							{previewUrl ? (
								<img src={previewUrl} alt={t`Stream preview`} className={styles.previewImage} />
							) : (
								<div className={styles.previewFallback}>{isPreviewLoading ? t`Loading...` : t`No preview yet`}</div>
							)}
							<div className={styles.previewWatchButton}>
								<Button
									variant="secondary"
									superCompact
									fitContent
									leftIcon={<ProjectorScreenIcon className={styles.watchStreamButtonIcon} />}
									onClick={handleWatch}
								>
									{t`Watch`}
								</Button>
							</div>
						</div>
					)}

					{isScreenShare && viewerUsers.length > 0 && (
						<div className={styles.viewersContainer}>
							<EyeIcon weight="fill" className={styles.viewersIcon} />
							<div className={styles.viewersAvatars}>
								{viewerUsers.slice(0, 5).map((u) => {
									const displayName = NicknameUtils.getNickname(u, guildId, channelId) || u.username || 'User';
									return (
										<Tooltip key={u.id} text={displayName} position="top">
											<div className={styles.viewerAvatarWrapper}>
												<Avatar user={u} size={24} guildId={guildId} />
											</div>
										</Tooltip>
									);
								})}
								{viewerUsers.length > 5 && (
									<Tooltip
										text={viewerUsers
											.slice(5)
											.map((u) => NicknameUtils.getNickname(u, guildId, channelId) || u.username || 'User')
											.join(', ')}
										position="top"
									>
										<div className={styles.viewerCountBadge}>+{viewerUsers.length - 5}</div>
									</Tooltip>
								)}
							</div>
						</div>
					)}

					{videoLocallyDisabled && (
						<div className={styles.videoDisabledOverlay}>
							<VideoCameraSlashIcon weight="fill" className={styles.videoDisabledIcon} />
						</div>
					)}

					{showFocusIndicator && isFocused && (
						<div className={styles.focusOverlay}>
							<EyeIcon weight="fill" className={styles.focusOverlayIcon} />
						</div>
					)}

					<div className={voiceCallStyles.lkParticipantMetadata}>
						<div className={voiceCallStyles.lkParticipantMetadataItem}>
							<div className={voiceCallStyles.lkParticipantIcons}>
								{((voiceState?.mute ?? false) || isSelfMuted) && (
									<Tooltip text={voiceState?.mute ? t`Community Muted` : t`Muted`} position="top">
										<MicrophoneSlashIcon
											weight="fill"
											className={voiceState?.mute ? styles.participantIconRed : styles.participantIconMuted}
										/>
									</Tooltip>
								)}

								{((voiceState?.deaf ?? false) || isSelfDeafened) && (
									<Tooltip text={voiceState?.deaf ? t`Community Deafened` : t`Deafened`} position="top">
										<SpeakerSlashIcon
											weight="fill"
											className={voiceState?.deaf ? styles.participantIconRed : styles.participantIconMuted}
										/>
									</Tooltip>
								)}

								<Tooltip text={voiceState?.is_mobile ? t`Mobile Device` : t`Desktop Device`} position="top">
									{voiceState?.is_mobile ? (
										<DeviceMobileIcon weight="regular" className={styles.participantIconWhite} />
									) : (
										<DesktopIcon weight="regular" className={styles.participantIconWhite} />
									)}
								</Tooltip>
							</div>

							<Tooltip text={participantDisplayName as string} position="top">
								<span className={clsx(styles.participantNameText, voiceCallStyles.participantName)}>
									{participantDisplayName}
								</span>
							</Tooltip>

							{connectionId && (
								<Tooltip text={t`Connection: ${connectionId}`} position="top">
									<span className={clsx(styles.participantConnectionText, voiceCallStyles.participantConn)}>
										({connectionId})
									</span>
								</Tooltip>
							)}
						</div>

						<FocusRing offset={-2}>
							<div
								role="button"
								tabIndex={0}
								className={clsx(voiceCallStyles.lkParticipantMetadataItem, styles.menuButton)}
								onClick={(e) => {
									e.stopPropagation();
									handleContextMenu(e);
								}}
								onKeyDown={handleMenuButtonKeyDown}
							>
								<DotsThreeIcon weight="bold" className={styles.menuButtonIcon} />
							</div>
						</FocusRing>
					</div>
				</LongPressable>
			</FocusRing>

			{isMobileExperience && participantUser && (
				<VoiceParticipantBottomSheet
					isOpen={bottomSheetOpen}
					onClose={() => setBottomSheetOpen(false)}
					user={participantUser}
					participant={connectionParticipant}
					guildId={guildId}
					connectionId={connectionId}
					isConnectionItem
				/>
			)}
		</>
	);
});
