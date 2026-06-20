// SPDX-License-Identifier: AGPL-3.0-or-later

import {ActivityCoverImage} from '@app/features/presence/components/ActivityCoverImage';
import {formatActivityDisplay} from '@app/features/presence/utils/formatActivityDisplay';
import {usePresenceActivities} from '@app/features/presence/hooks/usePresenceActivities';
import styles from '@app/features/user/components/profile/ProfileRichPresence.module.css';
import type {UserActivity} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {Trans} from '@lingui/react/macro';
import {GameControllerIcon, HeadphonesIcon} from '@phosphor-icons/react';
import type React from 'react';
import {useEffect, useMemo, useState} from 'react';

interface ProfileRichPresenceProps {
	userId: string;
}

function getActivityHeader(activity: UserActivity, listeningSource: string | null): React.ReactNode {
	switch (activity.type) {
		case 2:
			return listeningSource ? (
				<Trans comment="Rich presence header when listening to a music app or album">Listening to {listeningSource}</Trans>
			) : (
				<Trans comment="Rich presence header when listening to music with no app name">Listening to Music</Trans>
			);
		case 3:
			return <Trans comment="Rich presence header when watching something">Watching</Trans>;
		case 5:
			return <Trans comment="Rich presence header when competing in a game">Competing in</Trans>;
		default:
			return <Trans comment="Rich presence header on user profile">Playing</Trans>;
	}
}

function normalizeStartTimestamp(start?: number): number | undefined {
	if (start == null || !Number.isFinite(start)) return undefined;
	if (start > 10_000_000_000) return Math.floor(start / 1000);
	return start;
}

function formatElapsed(startSeconds: number, nowMs: number): string {
	const elapsed = Math.max(0, Math.floor(nowMs / 1000) - startSeconds);
	const hours = Math.floor(elapsed / 3600);
	const minutes = Math.floor((elapsed % 3600) / 60);
	const seconds = elapsed % 60;
	if (hours > 0) {
		return `${hours}:${String(minutes).padStart(2, '0')}:${String(seconds).padStart(2, '0')}`;
	}
	return `${minutes}:${String(seconds).padStart(2, '0')}`;
}

function ActivityTimer({start}: {start?: number}) {
	const startSeconds = normalizeStartTimestamp(start);
	const [now, setNow] = useState(Date.now());
	useEffect(() => {
		if (!startSeconds) return;
		const id = window.setInterval(() => setNow(Date.now()), 1000);
		return () => window.clearInterval(id);
	}, [startSeconds]);
	if (!startSeconds) return null;
	return <div className={styles.activityTimer}>{formatElapsed(startSeconds, now)} elapsed</div>;
}

function ActivityFallbackIcon({type}: {type: number}) {
	if (type === 2) {
		return <HeadphonesIcon className={styles.activityIconFallback} weight="fill" aria-hidden />;
	}
	return <GameControllerIcon className={styles.activityIconFallback} weight="fill" aria-hidden />;
}

export const ProfileRichPresence: React.FC<ProfileRichPresenceProps> = ({userId}) => {
	const activities = usePresenceActivities({userId});
	const activity = useMemo(() => activities[0] ?? null, [activities]);
	const display = useMemo(() => (activity ? formatActivityDisplay(activity) : null), [activity]);
	if (!activity || !display) return null;
	return (
		<div className={styles.activityCard} data-flx="user.profile.profile-rich-presence">
			<div className={styles.headerRow}>
				<span className={styles.activityLabel}>{getActivityHeader(activity, display.listeningSource)}</span>
			</div>
			<div className={styles.activityRow}>
				<div className={styles.activityArt}>
					<ActivityCoverImage
						activity={activity}
						className={styles.activityIcon}
						fallback={<ActivityFallbackIcon type={activity.type} />}
					/>
				</div>
				<div className={styles.activityBody}>
					<div className={styles.activityPrimary}>{display.primary}</div>
					{display.secondary ? <div className={styles.activitySecondary}>{display.secondary}</div> : null}
					<ActivityTimer start={activity.timestamps?.start} />
				</div>
			</div>
		</div>
	);
};
