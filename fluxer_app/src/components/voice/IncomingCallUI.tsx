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
import {PhoneIcon, PhoneIncomingIcon, XIcon} from '@phosphor-icons/react';
import {motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';
import {ChannelTypes} from '~/Constants';
import {GroupDMAvatar} from '~/components/common/GroupDMAvatar';
import {BottomSheet} from '~/components/uikit/BottomSheet/BottomSheet';
import {Button} from '~/components/uikit/Button/Button';
import {StatusAwareAvatar} from '~/components/uikit/StatusAwareAvatar';
import type {ChannelRecord} from '~/records/ChannelRecord';
import type {UserRecord} from '~/records/UserRecord';
import * as ChannelUtils from '~/utils/ChannelUtils';
import {isMobileExperienceEnabled} from '~/utils/mobileExperience';
import {INCOMING_CALL_OVERLAY_HEIGHT, INCOMING_CALL_OVERLAY_WIDTH} from './IncomingCallOverlayConstants';
import styles from './IncomingCallUI.module.css';

interface Position {
	x: number;
	y: number;
}

interface DragState {
	pointerId: number;
	startX: number;
	startY: number;
	offsetX: number;
	offsetY: number;
	dragging: boolean;
	lastPosition?: Position;
}

interface DragListeners {
	move: (event: PointerEvent) => void;
	up: (event: PointerEvent) => void;
}

const DRAG_START_THRESHOLD = 6;
const DRAG_START_THRESHOLD_SQ = DRAG_START_THRESHOLD ** 2;

function clamp(value: number, min: number, max: number): number {
	return Math.min(Math.max(value, min), max);
}

function clampPosition(x: number, y: number, maxX: number, maxY: number): Position {
	return {
		x: clamp(x, 0, maxX),
		y: clamp(y, 0, maxY),
	};
}

function getViewportWidth(): number {
	return window.innerWidth;
}

function getViewportHeight(): number {
	return window.innerHeight;
}

function resolveMaxDimension(value: number | undefined, viewport: number, overlaySize: number): number {
	if (typeof value === 'number') return Math.max(0, value);
	return Math.max(0, viewport - overlaySize);
}

export interface IncomingCallUIProps {
	channel: ChannelRecord | null;
	initiator: UserRecord | null;
	onAccept: () => void;
	onReject: () => void;
	onIgnore: () => void;
	initialX?: number;
	initialY?: number;
	maxX?: number;
	maxY?: number;
	onDragEnd?: (x: number, y: number) => void;
}

export const IncomingCallUI: React.FC<IncomingCallUIProps> = observer(
	({channel, initiator, onAccept, onReject, onIgnore, initialX, initialY, maxX, maxY, onDragEnd}) => {
		const {t} = useLingui();
		const [isDragging, setIsDragging] = useState(false);
		const pointerState = useRef<DragState | null>(null);
		const listenersRef = useRef<DragListeners | null>(null);
		const isMobileExperience = isMobileExperienceEnabled();

		const resolvedMaxX = useMemo(
			() => resolveMaxDimension(maxX, getViewportWidth(), INCOMING_CALL_OVERLAY_WIDTH),
			[maxX],
		);
		const resolvedMaxY = useMemo(
			() => resolveMaxDimension(maxY, getViewportHeight(), INCOMING_CALL_OVERLAY_HEIGHT),
			[maxY],
		);

		const defaultPosition = useMemo(
			() =>
				clampPosition(
					getViewportWidth() / 2 - INCOMING_CALL_OVERLAY_WIDTH / 2,
					getViewportHeight() / 2 - INCOMING_CALL_OVERLAY_HEIGHT / 2,
					resolvedMaxX,
					resolvedMaxY,
				),
			[isMobileExperience, resolvedMaxX, resolvedMaxY],
		);

		const resolvedInitialPosition = useMemo(
			() =>
				clampPosition(
					typeof initialX === 'number' ? initialX : defaultPosition.x,
					typeof initialY === 'number' ? initialY : defaultPosition.y,
					resolvedMaxX,
					resolvedMaxY,
				),
			[defaultPosition.x, defaultPosition.y, initialX, initialY, resolvedMaxX, resolvedMaxY],
		);

		const [position, setPosition] = useState<Position>(() => resolvedInitialPosition);

		useEffect(() => {
			if (!isDragging) setPosition(resolvedInitialPosition);
		}, [isDragging, resolvedInitialPosition]);

		useEffect(() => {
			if (!isDragging) return;

			const prev = document.body.style.cursor;
			document.body.style.cursor = 'grabbing';

			return () => {
				document.body.style.cursor = prev;
			};
		}, [isDragging]);

		const handlePointerMove = useCallback(
			(event: PointerEvent) => {
				if (isMobileExperience) return;
				const state = pointerState.current;
				if (!state || state.pointerId !== event.pointerId) return;

				const deltaX = event.clientX - state.startX;
				const deltaY = event.clientY - state.startY;

				if (!state.dragging) {
					if (deltaX * deltaX + deltaY * deltaY <= DRAG_START_THRESHOLD_SQ) return;
					state.dragging = true;
					setIsDragging(true);
				}

				const nextX = event.clientX - state.offsetX;
				const nextY = event.clientY - state.offsetY;
				const clamped = clampPosition(nextX, nextY, resolvedMaxX, resolvedMaxY);
				state.lastPosition = clamped;
				setPosition(clamped);
			},
			[resolvedMaxX, resolvedMaxY],
		);

		const cleanupListeners = useCallback(() => {
			const listeners = listenersRef.current;
			if (!listeners) return;

			window.removeEventListener('pointermove', listeners.move);
			window.removeEventListener('pointerup', listeners.up);
			window.removeEventListener('pointercancel', listeners.up);
			listenersRef.current = null;
		}, []);

		const handlePointerUp = useCallback(
			(event: PointerEvent) => {
				if (isMobileExperience) return;
				const state = pointerState.current;
				if (!state || state.pointerId !== event.pointerId) return;

				cleanupListeners();

				if (state.dragging) {
					setIsDragging(false);
					const finalPosition = state.lastPosition ?? position;
					setPosition(finalPosition);
					onDragEnd?.(finalPosition.x, finalPosition.y);
				}

				pointerState.current = null;
			},
			[cleanupListeners, isMobileExperience, onDragEnd, position],
		);

		const handleDragHandlePointerDown = useCallback(
			(event: React.PointerEvent<HTMLDivElement>) => {
				if (isMobileExperience || event.button !== 0) return;

				event.preventDefault();
				event.stopPropagation();

				pointerState.current = {
					pointerId: event.pointerId,
					startX: event.clientX,
					startY: event.clientY,
					offsetX: event.clientX - position.x,
					offsetY: event.clientY - position.y,
					dragging: false,
				};

				const moveListener = (moveEvent: PointerEvent) => handlePointerMove(moveEvent);
				const upListener = (upEvent: PointerEvent) => handlePointerUp(upEvent);

				listenersRef.current = {
					move: moveListener,
					up: upListener,
				};

				window.addEventListener('pointermove', moveListener);
				window.addEventListener('pointerup', upListener);
				window.addEventListener('pointercancel', upListener);
			},
			[handlePointerMove, handlePointerUp, isMobileExperience, position.x, position.y],
		);

		useEffect(() => {
			return () => {
				cleanupListeners();
			};
		}, [cleanupListeners]);

		const handleKeyDown = useCallback(
			(event: React.KeyboardEvent) => {
				if (event.key === 'Escape') {
					event.preventDefault();
					event.stopPropagation();
					onIgnore();
				}
			},
			[onIgnore],
		);

		const isDM = channel?.type === ChannelTypes.DM;
		const isGroupDM = channel?.type === ChannelTypes.GROUP_DM;

		const callerName = useMemo(() => {
			if (!channel) return '';
			if (isDM && initiator) return initiator.username;
			if (isGroupDM) return ChannelUtils.getDMDisplayName(channel);
			return channel.name ?? t`Incoming call`;
		}, [channel, initiator, isDM, isGroupDM, t]);

		const avatarNode = useMemo(() => {
			if (!channel) return null;
			if (isDM && initiator) return <StatusAwareAvatar user={initiator} size={80} />;
			if (isGroupDM) return <GroupDMAvatar channel={channel} size={80} />;
			return null;
		}, [channel, initiator, isDM, isGroupDM]);

		const rootStyle = useMemo<React.CSSProperties>(
			() => ({
				transform: `translate3d(${position.x}px, ${position.y}px, 0)`,
			}),
			[position.x, position.y],
		);

		if (!channel) return null;

		const callContent = (
			<div className={styles.stack}>
				<div className={styles.labelRow}>
					<PhoneIncomingIcon width={18} height={18} className={styles.incomingIcon} />
					<span className={styles.incomingLabel}>{t`Incoming call`}</span>
				</div>

				<div className={styles.avatarLarge}>{avatarNode}</div>
				<div className={styles.callerNameCenter} title={callerName}>
					{callerName}
				</div>

				<div className={styles.actionsColumn}>
					<Button
						variant="primary"
						onClick={onAccept}
						leftIcon={<PhoneIcon size={16} weight="fill" />}
						className={styles.actionButton}
						data-autofocus
					>
						{t`Accept`}
					</Button>
					<Button
						variant="danger-primary"
						onClick={onReject}
						leftIcon={<XIcon size={16} weight="bold" />}
						className={styles.actionButton}
					>
						{t`Reject`}
					</Button>
					<Button variant="secondary" onClick={onIgnore} className={styles.actionButton}>
						{t`Ignore`}
					</Button>
				</div>
			</div>
		);

		if (isMobileExperience) {
			return (
				<BottomSheet
					isOpen
					onClose={onIgnore}
					title={callerName || t`Incoming call`}
					snapPoints={[0.25, 0.6, 0.9]}
					surface="primary"
				>
					<div className={styles.bottomSheetStack}>{callContent}</div>
				</BottomSheet>
			);
		}

		return (
			<div
				className={styles.incomingCall}
				role="dialog"
				aria-label={t`Incoming call`}
				tabIndex={-1}
				onKeyDown={handleKeyDown}
				style={rootStyle}
			>
				<span className={styles.screenReaderOnly}>{t`Incoming call`}</span>

				<motion.div
					className={styles.card}
					initial={{opacity: 0, scale: 0.985}}
					animate={{opacity: 1, scale: 1}}
					exit={{opacity: 0, scale: 0.985}}
					transition={{duration: 0.14, ease: 'easeOut'}}
				>
					<div className={styles.dragHandle} onPointerDown={handleDragHandlePointerDown} aria-hidden="true">
						<div className={styles.dragPill} />
					</div>

					{callContent}
				</motion.div>
			</div>
		);
	},
);
