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

import type {I18n} from '@lingui/core';
import {UsersIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import {getStatusTypeLabel, type StatusType, StatusTypes} from '~/Constants';
import {type AvatarStatusLayout, getAvatarStatusLayout} from '~/components/uikit/AvatarStatusLayout';
import baseAvatarStyles from '~/components/uikit/BaseAvatar.module.css';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import i18n from '~/i18n';
import type {ChannelRecord} from '~/records/ChannelRecord';
import PresenceStore from '~/stores/PresenceStore';
import UserStore from '~/stores/UserStore';
import * as AvatarUtils from '~/utils/AvatarUtils';
import {getGroupDMAccentColor} from '~/utils/GroupDMColorUtils';
import {cdnUrl} from '~/utils/UrlUtils';
import styles from './GroupDMAvatar.module.css';

function computeGroupStatus(channel: ChannelRecord): string | null {
	const memberIds = new Set<string>(channel.recipientIds);
	if (memberIds.size === 0) return null;

	for (const id of memberIds) {
		if (PresenceStore.getStatus(id) === StatusTypes.ONLINE) {
			return StatusTypes.ONLINE;
		}
	}

	return null;
}

function renderGroupStatusDot(status: string | null, size: number, isTyping?: boolean, i18nInstance: I18n = i18n) {
	const layout = getAvatarStatusLayout(size);

	if (!layout.supportsStatus) return null;
	if (!status && !isTyping) return null;

	const baseStatus = status || StatusTypes.ONLINE;
	const renderableStatus = baseStatus === StatusTypes.INVISIBLE ? StatusTypes.OFFLINE : baseStatus;

	const statusColor = `var(--status-${renderableStatus})`;
	const statusLabel = getStatusTypeLabel(i18nInstance, renderableStatus);

	const typingMode = Boolean(isTyping);

	const bubbleWidth = typingMode ? layout.innerTypingWidth : layout.innerStatusWidth;
	const bubbleHeight = typingMode ? layout.innerTypingHeight : layout.innerStatusHeight;
	const bubbleRight = typingMode ? layout.innerTypingRight : layout.innerStatusRight;
	const bubbleBottom = typingMode ? layout.innerTypingBottom : layout.innerStatusBottom;

	return (
		<Tooltip text={statusLabel}>
			<div
				className={styles.statusDot}
				style={{
					right: bubbleRight,
					bottom: bubbleBottom,
					width: bubbleWidth,
					height: bubbleHeight,
					borderRadius: bubbleHeight / 2,
					display: 'flex',
					alignItems: 'center',
					justifyContent: 'center',
				}}
				role="img"
				aria-label={typingMode ? `${statusLabel} typing indicator` : `${statusLabel} status`}
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
					>
						<div className={baseAvatarStyles.typingDots}>
							{[0, 0.25, 0.5].map((delay, i) => (
								<div
									key={i}
									className={baseAvatarStyles.typingDot}
									style={{
										width: Math.min(layout.innerStatusWidth, layout.innerStatusHeight) * 0.25,
										height: Math.min(layout.innerStatusWidth, layout.innerStatusHeight) * 0.25,
										backgroundColor: 'white',
										borderRadius: '50%',
										animationDelay: `${delay}s`,
									}}
								/>
							))}
						</div>
					</div>
				) : (
					// biome-ignore lint/a11y/noSvgWithoutTitle: decorative SVG, parent has aria-label
					<svg width={layout.innerStatusWidth} height={layout.innerStatusHeight} viewBox="0 0 1 1" aria-hidden>
						<rect
							x={0}
							y={0}
							width={1}
							height={1}
							fill={statusColor}
							mask={`url(#svg-mask-status-${renderableStatus})`}
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

	if (r <= 0) return [];
	if (extendW <= 0) {
		return [<circle key="status-cutout" cx={cx} cy={cy} r={r} fill="black" />];
	}

	return [
		<circle key="typing-right-cap" cx={cx} cy={cy} r={r} fill="black" />,
		<rect key="typing-bridge" x={cx - extendW} y={cy - r} width={extendW} height={r * 2} fill="black" />,
		<circle key="typing-left-cap" cx={cx - extendW} cy={cy} r={r} fill="black" />,
	];
}

interface GroupDMAvatarProps {
	channel: ChannelRecord;
	size: number;
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
		const currentUser = UserStore.currentUser;
		const iconUrl = AvatarUtils.getChannelIconURL({id: channel.id, icon: channel.icon}, size * 2);
		const accentColor = React.useMemo(() => getGroupDMAccentColor(channel.id), [channel.id]);
		const shouldShowStatusIndicator = !disableStatusIndicator;
		const status = shouldShowStatusIndicator ? (statusOverride ?? computeGroupStatus(channel)) : null;
		const statusForIndicator = status === StatusTypes.ONLINE ? status : null;
		const groupMaskId = React.useId();

		if (iconUrl) {
			const layout = getAvatarStatusLayout(size);
			const shouldRenderStatusDot = shouldShowStatusIndicator && (statusForIndicator || isTyping);
			const hasCutout = layout.supportsStatus && shouldRenderStatusDot;
			const statusDot = shouldRenderStatusDot ? renderGroupStatusDot(statusForIndicator, size, isTyping) : null;

			return (
				<div className={styles.container} style={{width: size, height: size}}>
					<svg viewBox={`0 0 ${size} ${size}`} className={styles.iconImageContainer} aria-hidden role="presentation">
						<defs>
							<mask id={groupMaskId} maskUnits="userSpaceOnUse" x={0} y={0} width={size} height={size}>
								<circle cx={size / 2} cy={size / 2} r={size / 2} fill="white" />
								{hasCutout &&
									(isTyping ? (
										renderTypingCutouts(layout)
									) : (
										<circle cx={layout.cutoutCx} cy={layout.cutoutCy} r={layout.cutoutRadius} fill="black" />
									))}
							</mask>
						</defs>
						<image
							href={iconUrl}
							width={size}
							height={size}
							mask={`url(#${groupMaskId})`}
							preserveAspectRatio="xMidYMid slice"
						/>
					</svg>
					{statusDot}
				</div>
			);
		}

		if (channel.recipientIds.length === 0) {
			const shouldRenderStatusDot = shouldShowStatusIndicator && (statusForIndicator || isTyping);
			const statusDot = shouldRenderStatusDot ? renderGroupStatusDot(statusForIndicator, size, isTyping) : null;

			return (
				<div
					className={styles.defaultIconContainer}
					style={{
						width: size,
						height: size,
						backgroundColor: accentColor,
					}}
				>
					<UsersIcon weight="fill" className={styles.defaultIcon} style={{width: size * 0.5, height: size * 0.5}} />
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
		const shouldRenderStatusDot = shouldShowStatusIndicator && (statusForIndicator || isTyping);
		const statusDot = shouldRenderStatusDot ? renderGroupStatusDot(statusForIndicator, clusterSize, isTyping) : null;
		const avatarBorderSize = 2;

		return (
			<div className={styles.multiAvatarContainer} style={{width: clusterSize, height: clusterSize}}>
				<svg
					viewBox={`0 0 ${clusterSize} ${clusterSize}`}
					style={{position: 'absolute', inset: 0, overflow: 'visible'}}
					aria-hidden
					role="presentation"
				>
					<defs>
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
								cutouts.push(<circle key={`cutout-${j}`} cx={otherCx} cy={otherCy} r={otherR} fill="black" />);
							}

							const isBottomRight = (count === 2 && index === 1) || (count === 3 && index === 2);

							if (shouldRenderStatusDot && isBottomRight && layout.supportsStatus) {
								if (isTyping) {
									cutouts.push(...renderTypingCutouts(layout));
								} else if (layout.cutoutRadius > 0) {
									cutouts.push(
										<circle
											key="status-cutout"
											cx={layout.cutoutCx}
											cy={layout.cutoutCy}
											r={layout.cutoutRadius}
											fill="black"
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
								>
									<circle cx={cx} cy={cy} r={r} fill="white" />
									{cutouts}
								</mask>
							);
						})}
					</defs>

					{displayRecipientIds.map((userId, index) => {
						const user = UserStore.getUser(userId);
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
							/>
						);
					})}
				</svg>
				{statusDot}
			</div>
		);
	},
);
