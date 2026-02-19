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

import * as ContextMenuActionCreators from '@app/actions/ContextMenuActionCreators';
import {UserProfilePopout} from '@app/components/popouts/UserProfilePopout';
import {AvatarWithPresence} from '@app/components/uikit/avatars/AvatarWithPresence';
import {VoiceParticipantContextMenu} from '@app/components/uikit/context_menu/VoiceParticipantContextMenu';
import {Popout} from '@app/components/uikit/popout/Popout';
import {useTooltipPortalRoot} from '@app/components/uikit/tooltip/Tooltip';
import styles from '@app/components/voice/StreamSpectatorsPopout.module.css';
import type {SpectatorEntry} from '@app/components/voice/useStreamSpectators';
import {useMergeRefs} from '@app/hooks/useMergeRefs';
import type {UserRecord} from '@app/records/UserRecord';
import AccessibilityStore from '@app/stores/AccessibilityStore';
import MediaEngineStore from '@app/stores/voice/MediaEngineFacade';
import * as NicknameUtils from '@app/utils/NicknameUtils';
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
import {useLingui} from '@lingui/react/macro';
import {DesktopIcon, DeviceMobileIcon} from '@phosphor-icons/react';
import {AnimatePresence, type MotionStyle, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type {HTMLAttributes, ReactElement, Ref, SyntheticEvent} from 'react';
import {Children, cloneElement, useCallback, useEffect, useMemo, useRef, useState} from 'react';

interface StreamSpectatorsPopoutProps {
	viewerUsers: ReadonlyArray<UserRecord>;
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
	const {t} = useLingui();
	const portalRoot = useTooltipPortalRoot();
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

	const hoverDelay = useMemo(() => ({open: 200, close: 200}), []);
	const hoverSafePolygon = useMemo(() => safePolygon({buffer: 4, requireIntent: false}), []);
	const hover = useHover(context, {delay: hoverDelay, handleClose: hoverSafePolygon});
	const {getReferenceProps, getFloatingProps} = useInteractions([hover]);

	const child = Children.only(children);
	const referenceRefs = useMemo(() => [refs.setReference, child.props.ref], [refs.setReference, child.props.ref]);
	const mergedRef = useMergeRefs(referenceRefs);

	const stopPropagation = useCallback((event: SyntheticEvent) => {
		event.stopPropagation();
	}, []);

	const referenceProps = useMemo(() => getReferenceProps({ref: mergedRef}), [getReferenceProps, mergedRef]);

	const floatingProps = useMemo(
		() =>
			getFloatingProps({
				ref: refs.setFloating,
				onMouseDown: stopPropagation,
				onClick: stopPropagation,
			}),
		[getFloatingProps, refs.setFloating, stopPropagation],
	);

	const floatingStyles = useMemo(
		(): MotionStyle => ({
			position: strategy,
			left: x ?? 0,
			top: y ?? 0,
			zIndex: 'var(--z-index-tooltip)',
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
			{portalRoot && (
				<FloatingPortal root={portalRoot}>
					<AnimatePresence>
						{isOpen && (
							<motion.div
								{...floatingProps}
								style={floatingStyles}
								initial={AccessibilityStore.useReducedMotion ? FLOATING_INITIAL_REDUCED : FLOATING_INITIAL}
								animate={FLOATING_ANIMATE}
								exit={AccessibilityStore.useReducedMotion ? FLOATING_EXIT_REDUCED : FLOATING_EXIT}
								transition={AccessibilityStore.useReducedMotion ? FLOATING_TRANSITION_REDUCED : FLOATING_TRANSITION}
							>
								<div className={styles.card}>
									<div className={styles.header}>
										{t`Spectators`} - {count}
									</div>
									<div className={styles.list} role="listbox">
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
													/>
												))}
									</div>
								</div>
							</motion.div>
						)}
					</AnimatePresence>
				</FloatingPortal>
			)}
		</>
	);
});

interface SpectatorRowProps {
	user: UserRecord;
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
	const displayName = NicknameUtils.getNickname(user, guildId, channelId) || user.username || user.id;
	const participantName = displayName || user.username || user.id;
	const voiceState = connectionId ? MediaEngineStore.getVoiceStateByConnectionId(connectionId) : null;
	const selfMute = voiceState?.self_mute ?? false;
	const selfDeaf = voiceState?.self_deaf ?? false;

	const handleContextMenu = useCallback(
		(event: React.MouseEvent<HTMLElement>) => {
			event.preventDefault();
			event.stopPropagation();
			ContextMenuActionCreators.openFromEvent(event, ({onClose}) => (
				<VoiceParticipantContextMenu
					user={user}
					participantName={participantName}
					onClose={onClose}
					guildId={guildId}
					connectionId={connectionId}
				/>
			));
		},
		[channelId, connectionId, guildId, participantName, user],
	);

	return (
		<Popout
			render={({popoutKey}) => (
				<UserProfilePopout
					key={`${user.id}:${guildId ?? 'global'}:user`}
					popoutKey={popoutKey}
					user={user}
					isWebhook={false}
					guildId={guildId}
				/>
			)}
			position="left-start"
			onOpen={onPopoutOpen}
			onClose={onPopoutClose}
		>
			<div className={styles.spectatorRow} role="option" tabIndex={0} onContextMenu={handleContextMenu}>
				<AvatarWithPresence user={user} size={24} muted={selfMute} deafened={selfDeaf} guildId={guildId} />
				<span className={styles.spectatorName}>{displayName}</span>
				{connectionId != null && (
					<span className={styles.spectatorDevice}>
						{isMobile ? <DeviceMobileIcon size={14} weight="regular" /> : <DesktopIcon size={14} weight="regular" />}
					</span>
				)}
			</div>
		</Popout>
	);
});
