// SPDX-License-Identifier: AGPL-3.0-or-later

import {reportSkeletonCompactTimestampWidth} from '@app/features/app/components/skeleton/SkeletonLayoutMemory';
import {UserTag} from '@app/features/channel/components/ChannelUserTag';
import {MessageAvatar} from '@app/features/channel/components/MessageAvatar';
import {MessageTimeoutIndicator} from '@app/features/channel/components/MessageTimeoutIndicator';
import {MessageUsername} from '@app/features/channel/components/MessageUsername';
import {TimestampWithTooltip} from '@app/features/channel/components/TimestampWithTooltip';
import type {Guild} from '@app/features/guild/models/Guild';
import type {GuildMember} from '@app/features/member/models/GuildMember';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {compactMessagePrefixProps} from '@app/features/theme/layout/MessageLayoutAttributes';
import {getRemScaleForDocument} from '@app/features/theme/layout/RemFromPx';
import styles from '@app/features/theme/styles/Message.module.css';
import type {User} from '@app/features/user/models/User';
import * as DateUtils from '@app/features/user/utils/DateFormatting';
import type {MessagePreviewContext} from '@fluxer/constants/src/ChannelConstants';
import {clsx} from 'clsx';
import type React from 'react';
import {useCallback, useLayoutEffect, useRef} from 'react';

const COMPACT_TIMESTAMP_WIDTH_CSS_VARIABLE = '--message-compact-measured-timestamp-width';
const COMPACT_MESSAGE_SELECTOR = '[data-flx-compact="true"]';

interface CompactMessageLayoutProps {
	message: Message;
	shouldGroup: boolean;
	mobileLayoutEnabled: boolean;
	children: (showMetadata: boolean) => React.ReactNode;
}

interface CompactAuthorPrefixProps {
	message: Message;
	author: User;
	guild?: Guild;
	member?: GuildMember;
	showAvatar: boolean;
	showTimeoutIndicator: boolean;
	isHovering: boolean;
	previewContext?: keyof typeof MessagePreviewContext;
	previewOverrides?: {
		usernameColor?: string;
		displayName?: string;
	};
}

function shouldShowCompactMetadata(shouldGroup: boolean, mobileLayoutEnabled: boolean): boolean {
	return !(shouldGroup && mobileLayoutEnabled);
}

function useMeasuredCompactTimestampWidth(enabled: boolean): React.RefCallback<HTMLSpanElement> {
	const timestampRef = useRef<HTMLSpanElement | null>(null);
	const setTimestampRef = useCallback((node: HTMLSpanElement | null) => {
		timestampRef.current = node;
	}, []);

	useLayoutEffect(() => {
		if (!enabled) return undefined;
		const timestampElement = timestampRef.current;
		if (!timestampElement) return undefined;
		const messageElement = timestampElement.closest<HTMLElement>(COMPACT_MESSAGE_SELECTOR);
		if (!messageElement) return undefined;

		let animationFrame = 0;
		let lastWidth = '';
		const updateWidth = () => {
			animationFrame = 0;
			const width = timestampElement.getBoundingClientRect().width;
			const nextWidth = width > 0 ? `${width.toFixed(2)}px` : '';
			if (nextWidth === lastWidth) return;
			lastWidth = nextWidth;
			if (nextWidth) {
				reportSkeletonCompactTimestampWidth(width / getRemScaleForDocument(timestampElement.ownerDocument));
				messageElement.style.setProperty(COMPACT_TIMESTAMP_WIDTH_CSS_VARIABLE, nextWidth);
			} else {
				messageElement.style.removeProperty(COMPACT_TIMESTAMP_WIDTH_CSS_VARIABLE);
			}
		};
		const scheduleUpdate = () => {
			if (animationFrame) {
				cancelAnimationFrame(animationFrame);
			}
			animationFrame = requestAnimationFrame(updateWidth);
		};

		updateWidth();
		const resizeObserver = typeof ResizeObserver === 'undefined' ? undefined : new ResizeObserver(scheduleUpdate);
		resizeObserver?.observe(timestampElement);

		return () => {
			if (animationFrame) {
				cancelAnimationFrame(animationFrame);
			}
			resizeObserver?.disconnect();
			messageElement.style.removeProperty(COMPACT_TIMESTAMP_WIDTH_CSS_VARIABLE);
		};
	}, [enabled]);

	return setTimestampRef;
}

function CompactTimestamp({
	message,
	shouldGroup,
	timestampRef,
}: {
	message: Message;
	shouldGroup: boolean;
	timestampRef: React.Ref<HTMLSpanElement>;
}): React.ReactElement {
	const className = shouldGroup ? styles.messageTimestampCompactHover : styles.messageTimestampCompact;
	return (
		<TimestampWithTooltip
			date={message.timestamp}
			className={className}
			containerRef={timestampRef}
			data-flx="channel.compact-message-layout.compact-timestamp.timestamp-with-tooltip"
		>
			<span className={styles.copyOnly} data-flx="channel.compact-message-layout.compact-timestamp.copy-only">
				[
			</span>
			{DateUtils.getFormattedTime(message.timestamp)}
			<span className={styles.copyOnly} data-flx="channel.compact-message-layout.compact-timestamp.copy-only--2">
				]
			</span>
		</TimestampWithTooltip>
	);
}

export function CompactMessageLayout({
	message,
	shouldGroup,
	mobileLayoutEnabled,
	children,
}: CompactMessageLayoutProps): React.ReactElement {
	const showMetadata = shouldShowCompactMetadata(shouldGroup, mobileLayoutEnabled);
	const timestampRef = useMeasuredCompactTimestampWidth(showMetadata);
	return (
		<div
			className={clsx(styles.compactContentWrapper, !showMetadata && styles.compactContentNoPrefix)}
			data-flx="channel.compact-message-layout.compact-content"
		>
			{showMetadata && (
				<CompactTimestamp
					message={message}
					shouldGroup={shouldGroup}
					timestampRef={timestampRef}
					data-flx="channel.compact-message-layout.compact-timestamp"
				/>
			)}
			<div className={styles.compactBody} data-flx="channel.compact-message-layout.compact-body">
				{children(showMetadata)}
			</div>
		</div>
	);
}

export function CompactAuthorPrefix({
	message,
	author,
	guild,
	member,
	showAvatar,
	showTimeoutIndicator,
	isHovering,
	previewContext,
	previewOverrides,
}: CompactAuthorPrefixProps): React.ReactElement {
	const isPreview = Boolean(previewContext);
	return (
		<span
			className={styles.compactAuthorPrefix}
			data-flx="channel.compact-message-layout.compact-author-prefix.compact-author-prefix"
			{...compactMessagePrefixProps()}
		>
			<span className={styles.copyOnly} data-flx="channel.compact-message-layout.compact-author-prefix.copy-only">
				{' '}
			</span>
			{author.bot && (
				<UserTag
					className={styles.userTagCompact}
					system={author.system}
					data-flx="channel.compact-message-layout.compact-author-prefix.user-tag-compact"
				/>
			)}
			{showAvatar && (
				<MessageAvatar
					user={author}
					message={message}
					guildId={guild?.id}
					size={16}
					className={styles.messageAvatarCompact}
					isHovering={isHovering}
					isPreview={isPreview}
					data-flx="channel.compact-message-layout.compact-author-prefix.message-avatar-compact"
				/>
			)}
			<span
				className={styles.messageAuthorPart}
				data-flx="channel.compact-message-layout.compact-author-prefix.message-author-part"
			>
				{showTimeoutIndicator && (
					<MessageTimeoutIndicator
						guildId={message.guildId}
						userId={author.id}
						data-flx="channel.compact-message-layout.compact-author-prefix.message-timeout-indicator"
					/>
				)}
				<MessageUsername
					user={author}
					message={message}
					guild={guild}
					member={member}
					className={styles.messageUsername}
					isPreview={isPreview}
					previewColor={previewOverrides?.usernameColor}
					previewName={previewOverrides?.displayName}
					data-flx="channel.compact-message-layout.compact-author-prefix.message-username"
				/>
			</span>
			<span className={styles.copyOnly} data-flx="channel.compact-message-layout.compact-author-prefix.copy-only--2">
				:{' '}
			</span>
		</span>
	);
}
