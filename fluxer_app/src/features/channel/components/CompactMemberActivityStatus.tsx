// SPDX-License-Identifier: AGPL-3.0-or-later

import {usePresenceActivities} from '@app/features/presence/hooks/usePresenceActivities';
import {usePresenceCustomStatus} from '@app/features/presence/hooks/usePresenceCustomStatus';
import {
	formatActivityMemberListLine,
	type ActivityMemberListKind,
} from '@app/features/presence/utils/formatActivityDisplay';
import styles from '@app/features/channel/components/CompactMemberActivityStatus.module.css';
import {useTextOverflow} from '@app/features/ui/hooks/useTextOverflow';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import type {CustomStatus} from '@app/features/user/state/CustomStatus';
import {isCustomStatusExpired, normalizeCustomStatus} from '@app/features/user/state/CustomStatus';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {GameControllerIcon, HeadphonesIcon, TelevisionIcon, TrophyIcon} from '@phosphor-icons/react';
import clsx from 'clsx';
import {useMemo, useRef} from 'react';

interface CompactMemberActivityStatusProps {
	className?: string;
	customStatus?: CustomStatus | null;
	userId: string;
}

function hasVisibleCustomStatus(status: CustomStatus | null | undefined): boolean {
	const normalized = normalizeCustomStatus(status ?? null);
	if (!normalized || isCustomStatusExpired(normalized)) {
		return false;
	}
	return Boolean(normalized.text?.trim() || normalized.emojiName || normalized.emojiId);
}

function ActivityKindIcon({kind}: {kind: ActivityMemberListKind}) {
	const iconClassName = styles.activityIcon;
	switch (kind) {
		case 'listening':
			return <HeadphonesIcon className={iconClassName} weight="fill" aria-hidden />;
		case 'watching':
			return <TelevisionIcon className={iconClassName} weight="fill" aria-hidden />;
		case 'competing':
			return <TrophyIcon className={iconClassName} weight="fill" aria-hidden />;
		default:
			return <GameControllerIcon className={iconClassName} weight="fill" aria-hidden />;
	}
}

export function CompactMemberActivityStatus({
	className,
	customStatus,
	userId,
}: CompactMemberActivityStatusProps) {
	const {i18n} = useLingui();
	const containerRef = useRef<HTMLDivElement>(null);
	const shouldFetchCustomStatus = customStatus === undefined;
	const presenceCustomStatus = usePresenceCustomStatus({
		userId,
		enabled: shouldFetchCustomStatus,
	});
	const resolvedCustomStatus = shouldFetchCustomStatus ? presenceCustomStatus : (customStatus ?? null);
	const activities = usePresenceActivities({userId, enabled: !hasVisibleCustomStatus(resolvedCustomStatus)});
	const activity = activities[0] ?? null;
	const line = useMemo(() => (activity ? formatActivityMemberListLine(activity) : null), [activity]);
	const tooltipText = useMemo(() => {
		if (!line) return null;
		switch (line.kind) {
			case 'listening':
				return i18n._(msg`Listening to ${line.text}`);
			case 'watching':
				return i18n._(msg`Watching ${line.text}`);
			case 'competing':
				return i18n._(msg`Competing in ${line.text}`);
			default:
				return i18n._(msg`Playing ${line.text}`);
		}
	}, [i18n, line]);
	const isOverflowing = useTextOverflow(containerRef, {content: line?.text ?? null, measureTextRange: true});

	if (hasVisibleCustomStatus(resolvedCustomStatus) || !line) {
		return null;
	}

	const content = (
		<div
			ref={containerRef}
			className={clsx(styles.root, className)}
			data-flx="channel.compact-member-activity-status.content"
		>
			<ActivityKindIcon kind={line.kind} />
			<span className={styles.text} data-flx="channel.compact-member-activity-status.text">
				{line.text}
			</span>
		</div>
	);

	if (tooltipText && isOverflowing) {
		return (
			<Tooltip text={tooltipText} data-flx="channel.compact-member-activity-status.tooltip">
				{content}
			</Tooltip>
		);
	}

	return content;
}
