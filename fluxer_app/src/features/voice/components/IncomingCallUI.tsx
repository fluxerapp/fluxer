// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {GroupDMAvatar} from '@app/features/app/components/shared/GroupDMAvatar';
import type {Channel} from '@app/features/channel/models/Channel';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import {INCOMING_CALL_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {BottomSheet} from '@app/features/ui/bottom_sheet/BottomSheet';
import {Avatar as UserAvatar} from '@app/features/ui/components/Avatar';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {isMobileExperienceEnabled} from '@app/features/ui/utils/MobileExperience';
import {getReducedMotionProps} from '@app/features/ui/utils/ReducedMotionAnimation';
import type {User} from '@app/features/user/models/User';
import {
	INCOMING_CALL_OVERLAY_HEIGHT,
	INCOMING_CALL_OVERLAY_WIDTH,
} from '@app/features/voice/components/IncomingCallOverlayConstants';
import styles from '@app/features/voice/components/IncomingCallUI.module.css';
import {
	INCOMING_CALL_ACCEPT_ACTION_DESCRIPTOR,
	INCOMING_CALL_IGNORE_ACTION_DESCRIPTOR,
	INCOMING_CALL_REJECT_ACTION_DESCRIPTOR,
	VOICE_CALL_DESCRIPTOR,
} from '@app/features/voice/utils/VoiceMessageDescriptors';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {useLingui} from '@lingui/react/macro';
import {PhoneIcon, PhoneIncomingIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';

const CARD_MOTION = {
	initial: {opacity: 0, scale: 0.94, y: 8},
	animate: {opacity: 1, scale: 1, y: 0},
	exit: {opacity: 0, scale: 0.96, y: 4},
	transition: {duration: 0.22, ease: [0.32, 0.72, 0, 1] as [number, number, number, number]},
};

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

function clampPosition(x: number, y: number, maxX: number, maxY: number, minY: number): Position {
	return {
		x: clamp(x, 0, maxX),
		y: clamp(y, minY, Math.max(minY, maxY)),
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

function getTitleBarHeight(): number {
	if (typeof window === 'undefined') return 0;
	const value = getComputedStyle(document.documentElement).getPropertyValue('--native-titlebar-height');
	const parsed = Number.parseFloat(value);
	return Number.isFinite(parsed) ? parsed : 0;
}

interface RootProps extends Omit<React.HTMLAttributes<HTMLDivElement>, 'role'> {
	children: React.ReactNode;
}

const Root: React.FC<RootProps> = ({children, className, ...rest}) => (
	<div
		className={clsx(styles.root, className)}
		role="dialog"
		tabIndex={-1}
		data-incoming-call=""
		data-flx="voice.incoming-call-ui.root.root"
		{...rest}
	>
		{children}
	</div>
);

interface CardProps {
	children: React.ReactNode;
	animate?: boolean;
	className?: string;
	'data-flx'?: string;
}

const Card: React.FC<CardProps> = observer(({children, animate = true, className, ...rest}) => {
	const reduce = Accessibility.useReducedMotion;
	if (!animate) {
		return (
			<div className={clsx(styles.card, className)} data-flx="voice.incoming-call-ui.card.card" {...rest}>
				{children}
			</div>
		);
	}
	return (
		<motion.div
			className={clsx(styles.card, className)}
			data-flx="voice.incoming-call-ui.card.card--2"
			{...rest}
			{...getReducedMotionProps(CARD_MOTION, reduce)}
		>
			{children}
		</motion.div>
	);
});

interface DragHandleProps {
	onPointerDown: (event: React.PointerEvent<HTMLDivElement>) => void;
	'data-flx'?: string;
}

const DragHandle: React.FC<DragHandleProps> = ({onPointerDown, ...rest}) => (
	<div
		className={styles.dragHandle}
		onPointerDown={onPointerDown}
		aria-hidden="true"
		data-flx="voice.incoming-call-ui.drag-handle.drag-handle.pointer-down"
		{...rest}
	>
		<div className={styles.dragPill} data-flx="voice.incoming-call-ui.drag-handle.drag-pill" />
	</div>
);

interface StackProps {
	children: React.ReactNode;
	className?: string;
	'data-flx'?: string;
}

const Stack: React.FC<StackProps> = ({children, className, ...rest}) => (
	<div className={clsx(styles.stack, className)} data-flx="voice.incoming-call-ui.stack.stack" {...rest}>
		{children}
	</div>
);

interface HeaderProps {
	label?: string;
	icon?: React.ReactNode;
	'data-flx'?: string;
}

const Header: React.FC<HeaderProps> = ({label, icon, ...rest}) => {
	const {i18n} = useLingui();
	return (
		<div className={styles.header} data-flx="voice.incoming-call-ui.header.header" {...rest}>
			{icon ?? (
				<PhoneIncomingIcon
					size={remFromPx(14)}
					weight="fill"
					className={styles.headerIcon}
					data-flx="voice.incoming-call-ui.header.header-icon"
				/>
			)}
			<span data-flx="voice.incoming-call-ui.header.span">{label ?? i18n._(INCOMING_CALL_DESCRIPTOR)}</span>
		</div>
	);
};

interface AvatarProps {
	channel: Channel;
	initiator?: User | null;
	'data-flx'?: string;
}

const Avatar: React.FC<AvatarProps> = ({channel, initiator, ...rest}) => {
	let content: React.ReactNode = null;
	if (channel.type === ChannelTypes.DM && initiator) {
		content = <UserAvatar user={initiator} size={80} data-flx="voice.incoming-call-ui.avatar.user-avatar" />;
	} else if (channel.type === ChannelTypes.GROUP_DM) {
		content = <GroupDMAvatar channel={channel} size={80} data-flx="voice.incoming-call-ui.avatar.group-dm-avatar" />;
	}
	return (
		<div className={styles.avatar} data-flx="voice.incoming-call-ui.avatar.avatar" {...rest}>
			{content}
		</div>
	);
};

interface CallerInfoProps {
	name: string;
	subtitle?: string;
	'data-flx'?: string;
}

const CallerInfo: React.FC<CallerInfoProps> = ({name, subtitle, ...rest}) => {
	const {i18n} = useLingui();
	return (
		<div className={styles.callerInfo} data-flx="voice.incoming-call-ui.caller-info.caller-info" {...rest}>
			<div className={styles.callerName} data-flx="voice.incoming-call-ui.caller-info.caller-name">
				{name}
			</div>
			<div className={styles.callerSubtitle} data-flx="voice.incoming-call-ui.caller-info.caller-subtitle">
				{subtitle ?? i18n._(VOICE_CALL_DESCRIPTOR)}
			</div>
		</div>
	);
};

interface ActionsProps {
	children: React.ReactNode;
	'data-flx'?: string;
}

const Actions: React.FC<ActionsProps> = ({children, ...rest}) => (
	<div className={styles.actions} data-flx="voice.incoming-call-ui.actions.actions" {...rest}>
		{children}
	</div>
);

interface ActionGroupProps {
	children: React.ReactNode;
	'data-flx'?: string;
}

const ActionGroup: React.FC<ActionGroupProps> = ({children, ...rest}) => (
	<div className={styles.actionGroup} data-flx="voice.incoming-call-ui.action-group.action-group" {...rest}>
		{children}
	</div>
);

interface ActionLabelProps {
	children: React.ReactNode;
	'data-flx'?: string;
}

const ActionLabel: React.FC<ActionLabelProps> = ({children, ...rest}) => (
	<span className={styles.actionLabel} data-flx="voice.incoming-call-ui.action-label.action-label" {...rest}>
		{children}
	</span>
);

interface CircleButtonProps extends Omit<React.ButtonHTMLAttributes<HTMLButtonElement>, 'children'> {
	variant: 'accept' | 'reject';
	children?: React.ReactNode;
}

const CircleButton: React.FC<CircleButtonProps> = ({variant, children, className, type, ...rest}) => (
	<FocusRing data-flx="voice.incoming-call-ui.circle-button.focus-ring">
		<button
			type={type ?? 'button'}
			className={clsx(
				styles.circleButton,
				variant === 'accept' && styles.circleButtonAccept,
				variant === 'reject' && styles.circleButtonReject,
				className,
			)}
			data-flx="voice.incoming-call-ui.circle-button.circle-button"
			{...rest}
		>
			{children ??
				(variant === 'accept' ? (
					<PhoneIcon size={remFromPx(28)} weight="fill" data-flx="voice.incoming-call-ui.circle-button.phone-icon" />
				) : (
					<PhoneIcon
						size={remFromPx(28)}
						weight="fill"
						className={styles.circleButtonIconRotated}
						data-flx="voice.incoming-call-ui.circle-button.circle-button-icon-rotated"
					/>
				))}
		</button>
	</FocusRing>
);

interface IgnoreLinkProps extends Omit<React.ButtonHTMLAttributes<HTMLButtonElement>, 'children'> {
	children?: React.ReactNode;
}

const IgnoreLink: React.FC<IgnoreLinkProps> = ({children, className, type, ...rest}) => {
	const {i18n} = useLingui();
	return (
		<FocusRing data-flx="voice.incoming-call-ui.ignore-link.focus-ring">
			<button
				type={type ?? 'button'}
				className={clsx(styles.ignoreLink, className)}
				data-flx="voice.incoming-call-ui.ignore-link.ignore-link"
				{...rest}
			>
				{children ?? i18n._(INCOMING_CALL_IGNORE_ACTION_DESCRIPTOR)}
			</button>
		</FocusRing>
	);
};

interface ScreenReaderLabelProps {
	children: React.ReactNode;
	'data-flx'?: string;
}

const ScreenReaderLabel: React.FC<ScreenReaderLabelProps> = ({children, ...rest}) => (
	<span
		className={styles.screenReaderOnly}
		data-flx="voice.incoming-call-ui.screen-reader-label.screen-reader-only"
		{...rest}
	>
		{children}
	</span>
);

export interface IncomingCallUIProps {
	channel: Channel | null;
	initiator: User | null;
	onAccept: () => void;
	onReject: () => void;
	onIgnore: () => void;
	initialX?: number;
	initialY?: number;
	maxX?: number;
	maxY?: number;
	onDragEnd?: (x: number, y: number) => void;
}

const SmartIncomingCallUI: React.FC<IncomingCallUIProps> = observer(
	({channel, initiator, onAccept, onReject, onIgnore, initialX, initialY, maxX, maxY, onDragEnd}) => {
		const {i18n} = useLingui();
		const [isDragging, setIsDragging] = useState(false);
		const pointerState = useRef<DragState | null>(null);
		const listenersRef = useRef<DragListeners | null>(null);
		const positionRef = useRef<Position | null>(null);
		const pendingDragMoveRef = useRef<{pointerId: number; clientX: number; clientY: number} | null>(null);
		const dragFrameRef = useRef<number | null>(null);
		const isMobileExperience = isMobileExperienceEnabled();
		const resolvedMaxX = useMemo(
			() => resolveMaxDimension(maxX, getViewportWidth(), INCOMING_CALL_OVERLAY_WIDTH),
			[maxX],
		);
		const resolvedMaxY = useMemo(
			() => resolveMaxDimension(maxY, getViewportHeight(), INCOMING_CALL_OVERLAY_HEIGHT),
			[maxY],
		);
		const minY = useMemo(() => getTitleBarHeight(), [isMobileExperience]);
		const defaultPosition = useMemo(
			() =>
				clampPosition(
					getViewportWidth() / 2 - INCOMING_CALL_OVERLAY_WIDTH / 2,
					getViewportHeight() / 2 - INCOMING_CALL_OVERLAY_HEIGHT / 2,
					resolvedMaxX,
					resolvedMaxY,
					minY,
				),
			[isMobileExperience, resolvedMaxX, resolvedMaxY, minY],
		);
		const resolvedInitialPosition = useMemo(
			() =>
				clampPosition(
					typeof initialX === 'number' ? initialX : defaultPosition.x,
					typeof initialY === 'number' ? initialY : defaultPosition.y,
					resolvedMaxX,
					resolvedMaxY,
					minY,
				),
			[defaultPosition.x, defaultPosition.y, initialX, initialY, resolvedMaxX, resolvedMaxY, minY],
		);
		const [position, setPosition] = useState<Position>(() => resolvedInitialPosition);
		positionRef.current = position;
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
		const cancelPendingDragFrame = useCallback(() => {
			if (dragFrameRef.current !== null) {
				cancelAnimationFrame(dragFrameRef.current);
				dragFrameRef.current = null;
			}
		}, []);
		const flushPointerMove = useCallback(() => {
			dragFrameRef.current = null;
			const pendingMove = pendingDragMoveRef.current;
			pendingDragMoveRef.current = null;
			if (!pendingMove || isMobileExperience) return;
			const state = pointerState.current;
			if (!state || state.pointerId !== pendingMove.pointerId) return;
			const deltaX = pendingMove.clientX - state.startX;
			const deltaY = pendingMove.clientY - state.startY;
			if (!state.dragging) {
				if (deltaX * deltaX + deltaY * deltaY <= DRAG_START_THRESHOLD_SQ) return;
				state.dragging = true;
				setIsDragging(true);
			}
			const nextX = pendingMove.clientX - state.offsetX;
			const nextY = pendingMove.clientY - state.offsetY;
			const clamped = clampPosition(nextX, nextY, resolvedMaxX, resolvedMaxY, minY);
			state.lastPosition = clamped;
			positionRef.current = clamped;
			setPosition(clamped);
		}, [isMobileExperience, resolvedMaxX, resolvedMaxY, minY]);
		const handlePointerMove = useCallback(
			(event: PointerEvent) => {
				if (isMobileExperience) return;
				const state = pointerState.current;
				if (!state || state.pointerId !== event.pointerId) return;
				event.preventDefault();
				pendingDragMoveRef.current = {pointerId: event.pointerId, clientX: event.clientX, clientY: event.clientY};
				if (dragFrameRef.current !== null) return;
				dragFrameRef.current = requestAnimationFrame(flushPointerMove);
			},
			[flushPointerMove, isMobileExperience],
		);
		const cleanupListeners = useCallback(() => {
			const listeners = listenersRef.current;
			if (!listeners) return;
			window.removeEventListener('pointermove', listeners.move);
			window.removeEventListener('pointerup', listeners.up);
			window.removeEventListener('pointercancel', listeners.up);
			listenersRef.current = null;
			pendingDragMoveRef.current = null;
			cancelPendingDragFrame();
		}, [cancelPendingDragFrame]);
		const handlePointerUp = useCallback(
			(event: PointerEvent) => {
				if (isMobileExperience) return;
				const state = pointerState.current;
				if (!state || state.pointerId !== event.pointerId) return;
				if (dragFrameRef.current !== null) {
					cancelPendingDragFrame();
					flushPointerMove();
				}
				cleanupListeners();
				if (state.dragging) {
					setIsDragging(false);
					const finalPosition = state.lastPosition ?? positionRef.current ?? position;
					setPosition(finalPosition);
					onDragEnd?.(finalPosition.x, finalPosition.y);
				}
				pointerState.current = null;
			},
			[cancelPendingDragFrame, cleanupListeners, flushPointerMove, isMobileExperience, onDragEnd, position],
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
		const callerName = useMemo(() => {
			if (!channel) return '';
			if (channel.type === ChannelTypes.DM && initiator) return initiator.displayName;
			if (channel.type === ChannelTypes.GROUP_DM) return ChannelUtils.getDMDisplayName(channel);
			return channel.name ?? i18n._(INCOMING_CALL_DESCRIPTOR);
		}, [channel, initiator, i18n.locale]);
		const rootStyle = useMemo<React.CSSProperties>(
			() => ({
				transform: `translate3d(${position.x}px, ${position.y}px, 0)`,
			}),
			[position.x, position.y],
		);
		if (!channel) return null;
		const acceptLabel = i18n._(INCOMING_CALL_ACCEPT_ACTION_DESCRIPTOR);
		const rejectLabel = i18n._(INCOMING_CALL_REJECT_ACTION_DESCRIPTOR);
		const ignoreLabel = i18n._(INCOMING_CALL_IGNORE_ACTION_DESCRIPTOR);
		const dialogLabel = i18n._(INCOMING_CALL_DESCRIPTOR);
		const stack = (
			<Stack data-flx="voice.incoming-call-ui.stack">
				<Header data-flx="voice.incoming-call-ui.header" />
				<Avatar channel={channel} initiator={initiator} data-flx="voice.incoming-call-ui.avatar" />
				<CallerInfo name={callerName} data-flx="voice.incoming-call-ui.caller-info" />
				<Actions data-flx="voice.incoming-call-ui.actions">
					<ActionGroup data-flx="voice.incoming-call-ui.action-group.reject">
						<CircleButton
							variant="reject"
							onClick={onReject}
							aria-label={rejectLabel}
							data-flx="voice.incoming-call-ui.action-button.reject"
						/>
						<ActionLabel data-flx="voice.incoming-call-ui.action-label.reject">{rejectLabel}</ActionLabel>
					</ActionGroup>
					<ActionGroup data-flx="voice.incoming-call-ui.action-group.accept">
						<CircleButton
							variant="accept"
							onClick={onAccept}
							aria-label={acceptLabel}
							data-autofocus
							data-flx="voice.incoming-call-ui.action-button.accept"
						/>
						<ActionLabel data-flx="voice.incoming-call-ui.action-label.accept">{acceptLabel}</ActionLabel>
					</ActionGroup>
				</Actions>
				<IgnoreLink onClick={onIgnore} data-flx="voice.incoming-call-ui.action-button.ignore">
					{ignoreLabel}
				</IgnoreLink>
			</Stack>
		);
		if (isMobileExperience) {
			return (
				<BottomSheet
					isOpen
					onClose={onIgnore}
					title={callerName || dialogLabel}
					snapPoints={[0.25, 0.6, 0.9]}
					surface="primary"
					data-flx="voice.incoming-call-ui.bottom-sheet"
				>
					<div className={styles.bottomSheetStack} data-flx="voice.incoming-call-ui.bottom-sheet-stack">
						<Root data-flx="voice.incoming-call-ui.smart-incoming-call-ui.root">
							<Card animate={false} data-flx="voice.incoming-call-ui.bottom-sheet-card">
								{stack}
							</Card>
						</Root>
					</div>
				</BottomSheet>
			);
		}
		return (
			<Root aria-label={dialogLabel} onKeyDown={handleKeyDown} style={rootStyle} data-flx="voice.incoming-call-ui.root">
				<ScreenReaderLabel data-flx="voice.incoming-call-ui.screen-reader-only">{dialogLabel}</ScreenReaderLabel>
				<Card data-flx="voice.incoming-call-ui.card">
					<DragHandle onPointerDown={handleDragHandlePointerDown} data-flx="voice.incoming-call-ui.drag-handle" />
					{stack}
				</Card>
			</Root>
		);
	},
);
export const IncomingCallUI = Object.assign(SmartIncomingCallUI, {
	Root,
	Card,
	DragHandle,
	Stack,
	Header,
	Avatar,
	CallerInfo,
	Actions,
	ActionGroup,
	ActionLabel,
	CircleButton,
	IgnoreLink,
	ScreenReaderLabel,
});
