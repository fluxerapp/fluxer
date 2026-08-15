// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {useMergeRefs} from '@app/features/app/hooks/useMergeRefs';
import {VoiceParticipantContextMenu} from '@app/features/ui/action_menu/VoiceParticipantContextMenu';
import {AvatarWithPresence} from '@app/features/ui/avatars/AvatarWithPresence';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import {usePortalHost} from '@app/features/ui/overlay/PortalHostContext';
import {Popout} from '@app/features/ui/popover/PopoverPopout';
import {UserProfilePopout} from '@app/features/user/components/popouts/UserProfilePopout';
import {useUserProfileHoverPreload} from '@app/features/user/hooks/useUserProfileHoverPreload';
import type {User} from '@app/features/user/models/User';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import styles from '@app/features/voice/components/StreamSpectatorsPopout.module.css';
import type {SpectatorEntry} from '@app/features/voice/components/useStreamSpectators';
import {resolveVoiceParticipantAvatarEntryVoiceState} from '@app/features/voice/components/VoiceParticipantDisplayState';
import MediaEngine, {useMediaEngineVersion} from '@app/features/voice/engine/MediaEngineFacade';
import {isParticipantVoicePermissionMuted} from '@app/features/voice/utils/VoicePermissionUtils';
import {
	autoUpdate,
	FloatingPortal,
	flip,
	offset,
	safePolygon,
	shift,
	useFloating,
	useHover,
	useInteractions,
} from '@floating-ui/react';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {DesktopIcon, DeviceMobileIcon} from '@phosphor-icons/react';
import {AnimatePresence, type MotionStyle, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type {HTMLAttributes, ReactElement, Ref, SyntheticEvent} from 'react';
import {Children, cloneElement, useCallback, useEffect, useMemo, useRef, useState} from 'react';

const SPECTATORS_DESCRIPTOR = msg({
	message: 'Spectators',
	comment: 'Title of the screen-share spectators popout listing users watching the stream.',
});
const VIEW_PROFILE_FOR_DESCRIPTOR = msg({
	message: 'View profile for {displayName}',
	comment: "Aria label on an avatar in the spectators popout. {displayName} is the participant's display name.",
});

interface StreamSpectatorsPopoutProps {
	viewerUsers: ReadonlyArray<User>;
	spectatorEntries?: ReadonlyArray<SpectatorEntry>;
	guildId?: string;
	channelId?: string;
	onOpenChange?: (open: boolean) => void;
	children: ReactElement<HTMLAttributes<HTMLElement> & {ref?: Ref<HTMLElement>}>;
}

const FLOATING_INITIAL = {opacity: 0, scale: 0.98};
const FLOATING_INITIAL_REDUCED = {opacity: 1, scale: 1};
const FLOATING_ANIMATE = {opacity: 1, scale: 1};
const FLOATING_EXIT = {opacity: 0, scale: 0.98};
const FLOATING_EXIT_REDUCED = {opacity: 1, scale: 1};
const FLOATING_TRANSITION = {
	opacity: {duration: 0.1},
	scale: {type: 'spring' as const, damping: 25, stiffness: 500},
};
const FLOATING_TRANSITION_REDUCED = {duration: 0};
export const StreamSpectatorsPopout = observer(function StreamSpectatorsPopout({
	viewerUsers,
	spectatorEntries,
	guildId,
	channelId,
	onOpenChange,
	children,
}: StreamSpectatorsPopoutProps) {
	const {i18n} = useLingui();
	const portalRoot = usePortalHost();
	const [isOpen, setIsOpen] = useState(false);
	const [profilePopoutOpen, setProfilePopoutOpen] = useState(false);
	const profilePopoutOpenRef = useRef(false);
	useEffect(() => {
		profilePopoutOpenRef.current = profilePopoutOpen;
	}, [profilePopoutOpen]);
	const handleOpenChange = useCallback(
		(open: boolean) => {
			if (!open && profilePopoutOpenRef.current) return;
			setIsOpen(open);
			onOpenChange?.(open);
		},
		[onOpenChange],
	);
	const floatingMiddleware = useMemo(() => [offset(8), flip(), shift({padding: 8})], []);
	const {x, y, refs, strategy, context} = useFloating({
		open: isOpen,
		onOpenChange: handleOpenChange,
		placement: 'bottom-start',
		middleware: floatingMiddleware,
		whileElementsMounted: autoUpdate,
	});
	const hoverDelay = useMemo(() => ({open: 200, close: 500}), []);
	const hoverSafePolygon = useMemo(() => safePolygon({buffer: 4, requireIntent: false}), []);
	const hover = useHover(context, {delay: hoverDelay, handleClose: hoverSafePolygon});
	const {getReferenceProps, getFloatingProps} = useInteractions([hover]);
	const child = Children.only(children);
	const referenceRefs = useMemo(() => [refs.setReference, child.props.ref], [refs.setReference, child.props.ref]);
	const mergedRef = useMergeRefs(referenceRefs);
	const stopPropagation = useCallback((event: SyntheticEvent) => {
		event.stopPropagation();
	}, []);
	const handleFloatingPointerEnter = useCallback(
		(event: React.PointerEvent<HTMLElement>) => {
			if (event.pointerType === 'touch') return;
			handleOpenChange(true);
		},
		[handleOpenChange],
	);
	const referenceProps = useMemo(() => getReferenceProps({ref: mergedRef}), [getReferenceProps, mergedRef]);
	const floatingProps = useMemo(
		() =>
			getFloatingProps({
				ref: refs.setFloating,
				onPointerEnter: handleFloatingPointerEnter,
				onMouseDown: stopPropagation,
				onClick: stopPropagation,
			}),
		[getFloatingProps, handleFloatingPointerEnter, refs.setFloating, stopPropagation],
	);
	const floatingStyles = useMemo(
		(): MotionStyle => ({
			position: strategy,
			left: x ?? 0,
			top: y ?? 0,
			zIndex: 'var(--z-index-popout)',
			visibility: x === null || y === null ? 'hidden' : 'visible',
			pointerEvents: 'auto',
		}),
		[strategy, x, y],
	);
	const handleProfilePopoutOpen = useCallback(() => setProfilePopoutOpen(true), []);
	const handleProfilePopoutClose = useCallback(() => setProfilePopoutOpen(false), []);
	const entries = spectatorEntries ?? [];
	const count = entries.length > 0 ? entries.length : viewerUsers.length;
	if (count === 0) return children;
	return (
		<>
			{cloneElement(child, referenceProps)}
			<FloatingPortal root={portalRoot ?? undefined} data-flx="voice.stream-spectators-popout.floating-portal">
				<AnimatePresence data-flx="voice.stream-spectators-popout.animate-presence">
					{isOpen && (
						<motion.div
							data-flx="voice.stream-spectators-popout.div"
							{...floatingProps}
							style={floatingStyles}
							initial={Accessibility.useReducedMotion ? FLOATING_INITIAL_REDUCED : FLOATING_INITIAL}
							animate={FLOATING_ANIMATE}
							exit={Accessibility.useReducedMotion ? FLOATING_EXIT_REDUCED : FLOATING_EXIT}
							transition={Accessibility.useReducedMotion ? FLOATING_TRANSITION_REDUCED : FLOATING_TRANSITION}
						>
							<div className={styles.card} data-flx="voice.stream-spectators-popout.card">
								<div className={styles.header} data-flx="voice.stream-spectators-popout.header">
									{i18n._(SPECTATORS_DESCRIPTOR)} - {count}
								</div>
								<div
									className={styles.list}
									role="group"
									aria-label={i18n._(SPECTATORS_DESCRIPTOR)}
									data-flx="voice.stream-spectators-popout.list"
								>
									{entries.length > 0
										? entries.map((entry) => (
												<SpectatorRow
													key={`${entry.userId}_${entry.connectionId}`}
													user={entry.user}
													guildId={guildId}
													channelId={channelId}
													isMobile={entry.isMobile}
													connectionId={entry.connectionId}
													onPopoutOpen={handleProfilePopoutOpen}
													onPopoutClose={handleProfilePopoutClose}
													data-flx="voice.stream-spectators-popout.spectator-row"
												/>
											))
										: viewerUsers.map((user) => (
												<SpectatorRow
													key={user.id}
													user={user}
													guildId={guildId}
													channelId={channelId}
													onPopoutOpen={handleProfilePopoutOpen}
													onPopoutClose={handleProfilePopoutClose}
													data-flx="voice.stream-spectators-popout.spectator-row--2"
												/>
											))}
								</div>
							</div>
						</motion.div>
					)}
				</AnimatePresence>
			</FloatingPortal>
		</>
	);
});

interface SpectatorRowProps {
	user: User;
	guildId?: string;
	channelId?: string;
	isMobile?: boolean;
	connectionId?: string;
	onPopoutOpen: () => void;
	onPopoutClose: () => void;
}

const SpectatorRow = observer(function SpectatorRow({
	user,
	guildId,
	channelId,
	isMobile,
	connectionId,
	onPopoutOpen,
	onPopoutClose,
}: SpectatorRowProps) {
	const {i18n} = useLingui();
	useMediaEngineVersion();
	const displayName = NicknameUtils.getNickname(user, guildId, channelId);
	const participantName = displayName || user.id;
	const {scheduleProfilePreload, cancelProfilePreload} = useUserProfileHoverPreload({
		userId: user.id,
		guildId,
	});
	const voiceState = connectionId ? MediaEngine.getVoiceStateByConnectionId(connectionId) : null;
	const isCurrentUser = Users.getCurrentUser()?.id === user.id;
	const {selfMute, selfDeaf} = resolveVoiceParticipantAvatarEntryVoiceState({
		snapshot: {isLocal: false},
		voiceState,
		permissionMuted: isParticipantVoicePermissionMuted({voiceState, guildId, channelId, isCurrentUser}),
		localEffectiveSelfMute: false,
		localSelfDeaf: false,
	});
	const handleContextMenu = useCallback(
		(event: React.MouseEvent<HTMLElement>) => {
			event.preventDefault();
			event.stopPropagation();
			ContextMenuCommands.openFromEvent(event, ({onClose}) => (
				<VoiceParticipantContextMenu
					user={user}
					participantName={participantName}
					onClose={onClose}
					guildId={guildId}
					connectionId={connectionId}
					data-flx="voice.stream-spectators-popout.handle-context-menu.voice-participant-context-menu"
				/>
			));
		},
		[channelId, connectionId, guildId, participantName, user],
	);
	return (
		<Popout
			render={({popoutKey, onClose}) => (
				<UserProfilePopout
					key={`${user.id}:${guildId ?? 'global'}:user`}
					popoutKey={popoutKey}
					user={user}
					isWebhook={false}
					guildId={guildId}
					onClose={onClose}
					data-flx="voice.stream-spectators-popout.spectator-row.user-profile-popout"
				/>
			)}
			position="left-start"
			animationType="profile-slide"
			constrainHeight={false}
			freezePosition
			keepOpenOnTargetUnmount
			onOpen={onPopoutOpen}
			onClose={onPopoutClose}
			data-flx="voice.stream-spectators-popout.spectator-row.popout"
		>
			<div
				className={styles.spectatorRow}
				role="button"
				tabIndex={0}
				onMouseEnter={scheduleProfilePreload}
				onMouseLeave={cancelProfilePreload}
				onContextMenu={handleContextMenu}
				aria-label={i18n._(VIEW_PROFILE_FOR_DESCRIPTOR, {displayName})}
				data-flx="voice.stream-spectators-popout.spectator-row.spectator-row.context-menu"
			>
				<AvatarWithPresence
					user={user}
					size={24}
					muted={selfMute}
					deafened={selfDeaf}
					guildId={guildId}
					data-flx="voice.stream-spectators-popout.spectator-row.avatar-with-presence"
				/>
				<span className={styles.spectatorName} data-flx="voice.stream-spectators-popout.spectator-row.spectator-name">
					{displayName}
				</span>
				{connectionId != null && (
					<span
						className={styles.spectatorDevice}
						data-flx="voice.stream-spectators-popout.spectator-row.spectator-device"
					>
						{isMobile ? (
							<DeviceMobileIcon
								size={14}
								weight="regular"
								data-flx="voice.stream-spectators-popout.spectator-row.device-mobile-icon"
							/>
						) : (
							<DesktopIcon
								size={14}
								weight="regular"
								data-flx="voice.stream-spectators-popout.spectator-row.desktop-icon"
							/>
						)}
					</span>
				)}
			</div>
		</Popout>
	);
});
