// SPDX-License-Identifier: AGPL-3.0-or-later

import sharedStyles from '@app/features/app/components/shared/custom_status_display/CustomStatusDisplay.module.css';
import {buildCustomEmojiURL} from '@app/features/expressions/utils/CustomEmojiImageUrl';
import {getEmojiURL as getUnicodeEmojiURL} from '@app/features/expressions/utils/EmojiUtils';
import {usePresenceCustomStatus} from '@app/features/presence/hooks/usePresenceCustomStatus';
import {useTextOverflow} from '@app/features/ui/hooks/useTextOverflow';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import type {CustomStatus} from '@app/features/user/state/CustomStatus';
import {getCustomStatusText, isCustomStatusExpired, normalizeCustomStatus} from '@app/features/user/state/CustomStatus';
import clsx from 'clsx';
import {type ReactNode, useEffect, useMemo, useRef, useState} from 'react';

interface CompactMemberCustomStatusProps {
	className?: string;
	customStatus?: CustomStatus | null;
	userId?: string;
	showText?: boolean;
}

function sanitizeText(text: string): string {
	return text.replace(/[\r\n]+/g, ' ').trim();
}

function getStatusExpiryDelay(status: CustomStatus | null): number | null {
	if (!status?.expiresAt) {
		return null;
	}
	const expiresAtMs = Date.parse(status.expiresAt);
	if (Number.isNaN(expiresAtMs)) {
		return null;
	}
	return Math.max(0, expiresAtMs - Date.now());
}

export function hasVisibleCompactMemberCustomStatus(status: CustomStatus | null | undefined): boolean {
	const normalized = normalizeCustomStatus(status ?? null);
	if (!normalized || isCustomStatusExpired(normalized)) {
		return false;
	}
	if (normalized.emojiId != null || normalized.emojiName != null) {
		return true;
	}
	return sanitizeText(normalized.text ?? '').length > 0;
}

function renderEmoji(status: CustomStatus): ReactNode {
	if (status.emojiId) {
		return (
			<img
				src={buildCustomEmojiURL({id: status.emojiId, animated: false})}
				alt={status.emojiName ?? undefined}
				draggable={false}
				className={sharedStyles.statusEmoji}
				data-flx="channel.compact-member-custom-status.emoji"
			/>
		);
	}
	if (!status.emojiName) {
		return null;
	}
	const twemojiUrl = getUnicodeEmojiURL(status.emojiName);
	if (!twemojiUrl) {
		return null;
	}
	return (
		<img
			src={twemojiUrl}
			alt={status.emojiName}
			draggable={false}
			className={sharedStyles.statusEmoji}
			data-flx="channel.compact-member-custom-status.unicode-emoji"
		/>
	);
}

export function CompactMemberCustomStatus({
	className,
	customStatus,
	userId,
	showText = true,
}: CompactMemberCustomStatusProps) {
	const containerRef = useRef<HTMLDivElement>(null);
	const shouldFetchFromPresence = customStatus === undefined && userId !== undefined;
	const presenceStatus = usePresenceCustomStatus({
		userId: userId ?? '',
		enabled: shouldFetchFromPresence,
	});
	const status = shouldFetchFromPresence ? presenceStatus : (customStatus ?? null);
	const [, setExpiryTick] = useState(0);
	useEffect(() => {
		const normalized = normalizeCustomStatus(status);
		const delay = getStatusExpiryDelay(normalized);
		if (delay == null) {
			return;
		}
		const timer = window.setTimeout(() => setExpiryTick((tick) => tick + 1), delay);
		return () => window.clearTimeout(timer);
	}, [status]);
	const normalized = useMemo(() => {
		const nextStatus = normalizeCustomStatus(status);
		if (!nextStatus || isCustomStatusExpired(nextStatus)) {
			return null;
		}
		return nextStatus;
	}, [status]);
	const fullText = normalized ? getCustomStatusText(normalized) : null;
	const isOverflowing = useTextOverflow(containerRef, {content: fullText, measureTextRange: true});
	if (!normalized) {
		return null;
	}
	const text = normalized.text ? sanitizeText(normalized.text) : null;
	const emoji = renderEmoji(normalized);
	if (!emoji && (!showText || !text)) {
		return null;
	}
	const content = (
		<div
			ref={containerRef}
			className={clsx(sharedStyles.content, sharedStyles.singleLine, sharedStyles.constrained, className)}
			data-flx="channel.compact-member-custom-status.content"
		>
			{emoji}
			{showText && text && (
				<span className={sharedStyles.truncatedText} data-flx="channel.compact-member-custom-status.text">
					{text}
				</span>
			)}
		</div>
	);
	if (fullText && isOverflowing) {
		return (
			<Tooltip text={fullText} data-flx="channel.compact-member-custom-status.tooltip">
				{content}
			</Tooltip>
		);
	}
	return content;
}
