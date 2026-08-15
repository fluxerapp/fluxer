// SPDX-License-Identifier: AGPL-3.0-or-later

import i18n from '@app/app/I18n';
import styles from '@app/features/app/components/shared/GroupDMAvatar.module.css';
import {getStatusTypeLabel} from '@app/features/app/constants/AppConstants';
import type {Channel} from '@app/features/channel/models/Channel';
import {getGroupDMAccentColor} from '@app/features/channel/utils/GroupDMColorUtils';
import {cdnUrl} from '@app/features/messaging/utils/MessagingUrlUtils';
import Presence from '@app/features/presence/state/Presence';
import TransientPresence from '@app/features/presence/state/TransientPresence';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import type {AvatarStatusLayout} from '@app/features/ui/components/AvatarStatusLayout';
import {getAvatarStatusLayout} from '@app/features/ui/components/AvatarStatusLayout';
import baseAvatarStyles from '@app/features/ui/components/BaseAvatar.module.css';
import {
	TYPING_BRIDGE_RIGHT_SHIFT_RATIO,
	TYPING_DOT_GAP_RATIO,
	TYPING_DOT_SIZE_RATIO,
} from '@app/features/ui/constants/TypingConstants';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import Users from '@app/features/user/state/Users';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import type {MediaProxyImageSize} from '@fluxer/constants/src/MediaProxyImageSizes';
import type {StatusType} from '@fluxer/constants/src/StatusConstants';
import {isOfflineStatus, StatusTypes} from '@fluxer/constants/src/StatusConstants';
import type {I18n} from '@lingui/core';
import {useLingui} from '@lingui/react/macro';
import {UsersIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useId, useMemo} from 'react';

const GROUP_STATUS_PRIORITY: ReadonlyArray<StatusType> = [StatusTypes.ONLINE, StatusTypes.IDLE, StatusTypes.DND];

function getStatusWithTransientFallback(userId: string): StatusType {
	const presenceStatus = Presence.getStatus(userId);
	if (!isOfflineStatus(presenceStatus)) {
		return presenceStatus;
	}
	return TransientPresence.getStatus(userId);
}

function getStatusPriority(status: StatusType): number {
	const index = GROUP_STATUS_PRIORITY.indexOf(status);
	return index === -1 ? GROUP_STATUS_PRIORITY.length : index;
}

function computeGroupStatus(channel: Channel): StatusType | null {
	const memberIds = new Set<string>(channel.recipientIds);
	if (memberIds.size === 0) return null;
	let groupStatus: StatusType | null = null;
	for (const id of memberIds) {
		const status = getStatusWithTransientFallback(id);
		if (isOfflineStatus(status)) {
			continue;
		}
		if (groupStatus == null || getStatusPriority(status) < getStatusPriority(groupStatus)) {
			groupStatus = status;
		}
	}
	return groupStatus;
}

function renderGroupStatusDot(status: StatusType, size: number, isTyping?: boolean, i18nInstance: I18n = i18n) {
	const layout = getAvatarStatusLayout(size);
	if (!layout.supportsStatus) return null;
	const renderableStatus = status === StatusTypes.INVISIBLE ? StatusTypes.OFFLINE : status;
	const statusColor = `var(--status-${renderableStatus})`;
	const statusLabel = getStatusTypeLabel(i18nInstance, renderableStatus);
	const typingMode = Boolean(isTyping);
	const bubbleWidth = typingMode ? layout.innerTypingWidth : layout.innerStatusWidth;
	const bubbleHeight = typingMode ? layout.innerTypingHeight : layout.innerStatusHeight;
	const bubbleRight = typingMode ? layout.innerTypingRight : layout.innerStatusRight;
	const bubbleBottom = typingMode ? layout.innerTypingBottom : layout.innerStatusBottom;
	return (
		<Tooltip text={statusLabel} data-flx="app.group-dm-avatar.render-group-status-dot.tooltip">
			<div
				className={styles.statusDot}
				style={{
					right: remFromPx(bubbleRight),
					bottom: remFromPx(bubbleBottom),
					width: remFromPx(bubbleWidth),
					height: remFromPx(bubbleHeight),
					borderRadius: remFromPx(bubbleHeight / 2),
					display: 'flex',
					alignItems: 'center',
					justifyContent: 'center',
				}}
				role="img"
				aria-label={typingMode ? `${statusLabel} typing indicator` : `${statusLabel} status`}
				data-flx="app.group-dm-avatar.render-group-status-dot.status-dot"
			>
				{typingMode ? (
					<div
						style={{
							width: '100%',
							height: '100%',
							backgroundColor: statusColor,
							borderRadius: 'inherit',
							display: 'flex',
							alignItems: 'center',
							justifyContent: 'center',
							overflow: 'hidden',
						}}
						data-flx="app.group-dm-avatar.render-group-status-dot.div"
					>
						<div
							className={baseAvatarStyles.typingDots}
							style={{
								gap: remFromPx(
									Math.round(Math.min(layout.innerTypingWidth, layout.innerTypingHeight) * TYPING_DOT_GAP_RATIO),
								),
							}}
							data-flx="app.group-dm-avatar.render-group-status-dot.div--2"
						>
							{[0, 0.25, 0.5].map((delay, i) => (
								<div
									key={i}
									className={baseAvatarStyles.typingDot}
									style={{
										width: remFromPx(
											Math.round(Math.min(layout.innerTypingWidth, layout.innerTypingHeight) * TYPING_DOT_SIZE_RATIO),
										),
										height: remFromPx(
											Math.round(Math.min(layout.innerTypingWidth, layout.innerTypingHeight) * TYPING_DOT_SIZE_RATIO),
										),
										backgroundColor: 'white',
										borderRadius: '50%',
										animationDelay: `${delay}s`,
									}}
									data-flx="app.group-dm-avatar.render-group-status-dot.div--3"
								/>
							))}
						</div>
					</div>
				) : (
					<svg
						width={layout.innerStatusWidth}
						height={layout.innerStatusHeight}
						viewBox="0 0 1 1"
						aria-hidden
						data-flx="app.group-dm-avatar.render-group-status-dot.svg"
					>
						<rect
							x={0}
							y={0}
							width={1}
							height={1}
							fill={statusColor}
							mask={`url(#svg-mask-status-${renderableStatus})`}
							data-flx="app.group-dm-avatar.render-group-status-dot.rect"
						/>
					</svg>
				)}
			</div>
		</Tooltip>
	);
}

function renderTypingCutouts(layout: AvatarStatusLayout): Array<React.ReactNode> {
	const extendW = Math.max(0, layout.innerTypingWidth - layout.innerStatusWidth);
	const cx = layout.cutoutCx;
	const cy = layout.cutoutCy;
	const r = layout.cutoutRadius;
	const typingBridgeShift = extendW * TYPING_BRIDGE_RIGHT_SHIFT_RATIO;
	const bridgeX = cx - extendW + typingBridgeShift;
	const typingRightCapX = cx + typingBridgeShift;
	if (r <= 0) return [];
	if (extendW <= 0) {
		return [
			<circle
				key="status-cutout"
				cx={cx}
				cy={cy}
				r={r}
				fill="black"
				data-flx="app.group-dm-avatar.render-typing-cutouts.circle"
			/>,
		];
	}
	return [
		<circle
			key="typing-right-cap"
			cx={typingRightCapX}
			cy={cy}
			r={r}
			fill="black"
			data-flx="app.group-dm-avatar.render-typing-cutouts.circle--2"
		/>,
		<rect
			key="typing-bridge"
			x={bridgeX}
			y={cy - r}
			width={extendW}
			height={r * 2}
			fill="black"
			data-flx="app.group-dm-avatar.render-typing-cutouts.rect"
		/>,
		<circle
			key="typing-left-cap"
			cx={bridgeX}
			cy={cy}
			r={r}
			fill="black"
			data-flx="app.group-dm-avatar.render-typing-cutouts.circle--3"
		/>,
	];
}

interface GroupDMAvatarProps {
	channel: Channel;
	size: MediaProxyImageSize;
	isTyping?: boolean;
	disableStatusIndicator?: boolean;
	statusOverride?: StatusType | null;
}

interface AvatarPosition {
	top: number;
	left: number;
	avatarSize: number;
}

function getAvatarPosition(count: number, index: number, size: number): AvatarPosition {
	let top = 0;
	let left = 0;
	let avatarSize = size;
	if (count === 2) {
		const ratio = 0.7;
		avatarSize = size * ratio;
		const verticalInset = Math.min(size * 0.06, avatarSize * 0.18);
		if (index === 0) {
			top = verticalInset;
			left = 0;
		} else {
			top = size - avatarSize - verticalInset;
			left = size - avatarSize;
		}
		return {top, left, avatarSize};
	}
	if (count === 3) {
		const ratio = 0.68;
		avatarSize = size * ratio;
		const verticalInset = Math.min(size * 0.04, avatarSize * 0.12);
		const topRowTop = verticalInset;
		const bottomRowTop = size - avatarSize - verticalInset;
		const topLeft = 0;
		const topRight = size - avatarSize;
		const bottomCenter = (size - avatarSize) / 2;
		if (index === 0) {
			top = topRowTop;
			left = topLeft;
		} else if (index === 1) {
			top = topRowTop;
			left = topRight;
		} else {
			top = bottomRowTop;
			left = bottomCenter;
		}
		return {top, left, avatarSize};
	}
	return {top: 0, left: 0, avatarSize: size};
}

export const GroupDMAvatar: React.FC<GroupDMAvatarProps> = observer(
	({channel, size, isTyping = false, disableStatusIndicator = false, statusOverride}) => {
		const {i18n} = useLingui();
		const currentUser = Users.currentUser;
		const iconUrl = AvatarUtils.getChannelIconURL({id: channel.id, icon: channel.icon});
		const accentColor = useMemo(() => getGroupDMAccentColor(channel.id), [channel.id]);
		const shouldShowStatusIndicator = !disableStatusIndicator;
		const status = shouldShowStatusIndicator ? (statusOverride ?? computeGroupStatus(channel)) : null;
		const statusForIndicator = status != null && !isOfflineStatus(status) ? status : null;
		const shouldShowTypingIndicator = isTyping && statusForIndicator != null;
		const groupMaskId = useId();
		if (iconUrl) {
			const layout = getAvatarStatusLayout(size);
			const shouldRenderStatusDot = shouldShowStatusIndicator && statusForIndicator != null;
			const hasCutout = layout.supportsStatus && shouldRenderStatusDot;
			const statusDot =
				statusForIndicator != null && shouldRenderStatusDot
					? renderGroupStatusDot(statusForIndicator, size, shouldShowTypingIndicator, i18n)
					: null;
			return (
				<div
					className={styles.container}
					style={{width: remFromPx(size), height: remFromPx(size)}}
					data-flx="app.group-dm-avatar.container"
				>
					<svg
						viewBox={`0 0 ${size} ${size}`}
						className={styles.iconImageContainer}
						aria-hidden
						role="presentation"
						data-flx="app.group-dm-avatar.icon-image-container"
					>
						<defs data-flx="app.group-dm-avatar.defs">
							<mask
								id={groupMaskId}
								maskUnits="userSpaceOnUse"
								x={0}
								y={0}
								width={size}
								height={size}
								data-flx="app.group-dm-avatar.mask"
							>
								<circle cx={size / 2} cy={size / 2} r={size / 2} fill="white" data-flx="app.group-dm-avatar.circle" />
								{hasCutout &&
									(shouldShowTypingIndicator ? (
										renderTypingCutouts(layout)
									) : (
										<circle
											cx={layout.cutoutCx}
											cy={layout.cutoutCy}
											r={layout.cutoutRadius}
											fill="black"
											data-flx="app.group-dm-avatar.circle--2"
										/>
									))}
							</mask>
						</defs>
						<image
							href={iconUrl}
							width={size}
							height={size}
							mask={`url(#${groupMaskId})`}
							preserveAspectRatio="xMidYMid slice"
							data-flx="app.group-dm-avatar.image"
						/>
					</svg>
					{statusDot}
				</div>
			);
		}
		if (channel.recipientIds.length === 0) {
			const shouldRenderStatusDot = shouldShowStatusIndicator && statusForIndicator != null;
			const statusDot =
				statusForIndicator != null && shouldRenderStatusDot
					? renderGroupStatusDot(statusForIndicator, size, shouldShowTypingIndicator, i18n)
					: null;
			return (
				<div
					className={styles.defaultIconContainer}
					style={{
						width: remFromPx(size),
						height: remFromPx(size),
						backgroundColor: accentColor,
					}}
					data-flx="app.group-dm-avatar.default-icon-container"
				>
					<UsersIcon
						weight="fill"
						className={styles.defaultIcon}
						style={{width: remFromPx(size * 0.5), height: remFromPx(size * 0.5)}}
						data-flx="app.group-dm-avatar.default-icon"
					/>
					{statusDot}
				</div>
			);
		}
		const displayRecipientIds =
			channel.recipientIds.length === 1 && currentUser
				? [channel.recipientIds[0], currentUser.id]
				: channel.recipientIds.slice(0, 3);
		const count = displayRecipientIds.length;
		const clusterSize = count === 3 ? Math.min(size, 32) : size;
		const layout = getAvatarStatusLayout(clusterSize);
		const shouldRenderStatusDot = shouldShowStatusIndicator && statusForIndicator != null;
		const statusDot =
			statusForIndicator != null && shouldRenderStatusDot
				? renderGroupStatusDot(statusForIndicator, clusterSize, shouldShowTypingIndicator, i18n)
				: null;
		const avatarBorderSize = 2;
		return (
			<div
				className={styles.multiAvatarContainer}
				style={{width: remFromPx(clusterSize), height: remFromPx(clusterSize)}}
				data-flx="app.group-dm-avatar.multi-avatar-container"
			>
				<svg
					viewBox={`0 0 ${clusterSize} ${clusterSize}`}
					style={{position: 'absolute', inset: 0, overflow: 'visible'}}
					aria-hidden
					role="presentation"
					data-flx="app.group-dm-avatar.presentation"
				>
					<defs data-flx="app.group-dm-avatar.defs--2">
						{displayRecipientIds.map((userId, index) => {
							const {top, left, avatarSize} = getAvatarPosition(count, index, clusterSize);
							const avatarMaskId = `${groupMaskId}-avatar-${index}`;
							const cx = left + avatarSize / 2;
							const cy = top + avatarSize / 2;
							const r = avatarSize / 2;
							const cutouts: Array<React.ReactNode> = [];
							for (let j = index + 1; j < displayRecipientIds.length; j++) {
								const otherPos = getAvatarPosition(count, j, clusterSize);
								const otherCx = otherPos.left + otherPos.avatarSize / 2;
								const otherCy = otherPos.top + otherPos.avatarSize / 2;
								const otherR = otherPos.avatarSize / 2 + avatarBorderSize;
								cutouts.push(
									<circle
										key={`cutout-${j}`}
										cx={otherCx}
										cy={otherCy}
										r={otherR}
										fill="black"
										data-flx="app.group-dm-avatar.circle--3"
									/>,
								);
							}
							const isBottomRight = (count === 2 && index === 1) || (count === 3 && index === 2);
							if (shouldRenderStatusDot && isBottomRight && layout.supportsStatus) {
								if (shouldShowTypingIndicator) {
									cutouts.push(...renderTypingCutouts(layout));
								} else if (layout.cutoutRadius > 0) {
									cutouts.push(
										<circle
											key="status-cutout"
											cx={layout.cutoutCx}
											cy={layout.cutoutCy}
											r={layout.cutoutRadius}
											fill="black"
											data-flx="app.group-dm-avatar.circle--4"
										/>,
									);
								}
							}
							return (
								<mask
									key={userId}
									id={avatarMaskId}
									maskUnits="userSpaceOnUse"
									x={0}
									y={0}
									width={clusterSize}
									height={clusterSize}
									data-flx="app.group-dm-avatar.mask--2"
								>
									<circle cx={cx} cy={cy} r={r} fill="white" data-flx="app.group-dm-avatar.circle--5" />
									{cutouts}
								</mask>
							);
						})}
					</defs>
					{displayRecipientIds.map((userId, index) => {
						const user = Users.getUser(userId);
						const {top, left, avatarSize} = getAvatarPosition(count, index, clusterSize);
						const avatarMaskId = `${groupMaskId}-avatar-${index}`;
						let avatarUrl: string;
						if (user) {
							avatarUrl = AvatarUtils.getUserAvatarURL({id: user.id, avatar: user.avatar});
						} else {
							const avatarIndex = index % 6;
							avatarUrl = cdnUrl(`avatars/${avatarIndex}.png`);
						}
						return (
							<image
								key={userId}
								href={avatarUrl}
								x={left}
								y={top}
								width={avatarSize}
								height={avatarSize}
								mask={`url(#${avatarMaskId})`}
								preserveAspectRatio="xMidYMid slice"
								clipPath={`circle(${avatarSize / 2}px at ${avatarSize / 2}px ${avatarSize / 2}px)`}
								data-flx="app.group-dm-avatar.image--2"
							/>
						);
					})}
				</svg>
				{statusDot}
			</div>
		);
	},
);
