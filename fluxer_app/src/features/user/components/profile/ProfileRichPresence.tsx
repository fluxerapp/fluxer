// SPDX-License-Identifier: AGPL-3.0-or-later

import {ActivityCoverImage} from '@app/features/presence/components/ActivityCoverImage';
import {ExternalLink} from '@app/features/app/components/shared/ExternalLink';
import {formatActivityDisplay} from '@app/features/presence/utils/formatActivityDisplay';
import {usePresenceActivities} from '@app/features/presence/hooks/usePresenceActivities';
import styles from '@app/features/user/components/profile/ProfileRichPresence.module.css';
import type {UserActivity} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {Trans} from '@lingui/react/macro';
import {CaretLeftIcon, CaretRightIcon, GameControllerIcon, HeadphonesIcon} from '@phosphor-icons/react';
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

function formatRemaining(endSeconds: number, nowMs: number): string {
	const remaining = Math.max(0, endSeconds - Math.floor(nowMs / 1000));
	const hours = Math.floor(remaining / 3600);
	const minutes = Math.floor((remaining % 3600) / 60);
	const seconds = remaining % 60;
	if (hours > 0) {
		return `${hours}:${String(minutes).padStart(2, '0')}:${String(seconds).padStart(2, '0')}`;
	}
	return `${minutes}:${String(seconds).padStart(2, '0')}`;
}

function getProgressPercent(startSeconds: number, endSeconds: number, nowMs: number): number {
	const nowSeconds = Math.floor(nowMs / 1000);
	const duration = endSeconds - startSeconds;
	if (duration <= 0) return 0;
	const elapsed = nowSeconds - startSeconds;
	return Math.max(0, Math.min(100, (elapsed / duration) * 100));
}

function normalizeEndTimestamp(end?: number): number | undefined {
	if (end == null || !Number.isFinite(end)) return undefined;
	if (end > 10_000_000_000) return Math.floor(end / 1000);
	return end;
}

function ActivityTimer({start, end}: {start?: number; end?: number}) {
	const startSeconds = normalizeStartTimestamp(start);
	const endSeconds = normalizeEndTimestamp(end);
	const [now, setNow] = useState(Date.now());
	useEffect(() => {
		if (!startSeconds && !endSeconds) return;
		const id = window.setInterval(() => setNow(Date.now()), 1000);
		return () => window.clearInterval(id);
	}, [endSeconds, startSeconds]);
	if (startSeconds && endSeconds && endSeconds > Math.floor(now / 1000)) {
		return (
			<div className={styles.activityTimer}>
				{formatElapsed(startSeconds, now)} elapsed • {formatRemaining(endSeconds, now)} left
			</div>
		);
	}
	if (startSeconds) {
		return <div className={styles.activityTimer}>{formatElapsed(startSeconds, now)} elapsed</div>;
	}
	if (endSeconds && endSeconds > Math.floor(now / 1000)) {
		return <div className={styles.activityTimer}>{formatRemaining(endSeconds, now)} left</div>;
	}
	return null;
}

function ActivityProgress({start, end}: {start?: number; end?: number}) {
	const startSeconds = normalizeStartTimestamp(start);
	const endSeconds = normalizeEndTimestamp(end);
	const [now, setNow] = useState(Date.now());
	useEffect(() => {
		if (!startSeconds || !endSeconds) return;
		const id = window.setInterval(() => setNow(Date.now()), 1000);
		return () => window.clearInterval(id);
	}, [endSeconds, startSeconds]);
	if (!startSeconds || !endSeconds || endSeconds <= startSeconds) return null;
	return (
		<div className={styles.activityProgress} aria-hidden>
			<div
				className={styles.activityProgressFill}
				style={{width: `${getProgressPercent(startSeconds, endSeconds, now)}%`}}
			/>
		</div>
	);
}

function ActivityFallbackIcon({type}: {type: number}) {
	if (type === 2) {
		return <HeadphonesIcon className={styles.activityIconFallback} weight="fill" aria-hidden />;
	}
	return <GameControllerIcon className={styles.activityIconFallback} weight="fill" aria-hidden />;
}

function ActivityLine({
	className,
	href,
	children,
}: {
	className: string;
	href?: string;
	children: React.ReactNode;
}) {
	if (!href) return <div className={className}>{children}</div>;
	return (
		<ExternalLink href={href} className={`${className} ${styles.activityLink}`}>
			{children}
		</ExternalLink>
	);
}

export const ProfileRichPresence: React.FC<ProfileRichPresenceProps> = ({userId}) => {
	const activities = usePresenceActivities({userId});
	const [activityIndex, setActivityIndex] = useState(0);
	useEffect(() => {
		setActivityIndex((currentIndex) => Math.min(currentIndex, Math.max(activities.length - 1, 0)));
	}, [activities.length]);
	const hasMultipleActivities = activities.length > 1;
	const activity = useMemo(() => activities[activityIndex] ?? null, [activities, activityIndex]);
	const display = useMemo(() => (activity ? formatActivityDisplay(activity) : null), [activity]);
	if (!activity || !display) return null;
	return (
		<div className={styles.activityCard} data-flx="user.profile.profile-rich-presence">
			<div className={styles.headerRow}>
				<span className={styles.activityLabel}>{getActivityHeader(activity, display.listeningSource)}</span>
				{hasMultipleActivities ? (
					<div className={styles.activityCarouselControls}>
						<button
							type="button"
							className={styles.activityCarouselButton}
							onClick={() => setActivityIndex((currentIndex) => Math.max(currentIndex - 1, 0))}
							disabled={activityIndex === 0}
							aria-label="Show previous activity"
						>
							<CaretLeftIcon size={14} weight="bold" aria-hidden />
						</button>
						<span className={styles.activityCarouselCount}>
							{activityIndex + 1}/{activities.length}
						</span>
						<button
							type="button"
							className={styles.activityCarouselButton}
							onClick={() => setActivityIndex((currentIndex) => Math.min(currentIndex + 1, activities.length - 1))}
							disabled={activityIndex >= activities.length - 1}
							aria-label="Show next activity"
						>
							<CaretRightIcon size={14} weight="bold" aria-hidden />
						</button>
					</div>
				) : null}
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
					<ActivityLine className={styles.activityPrimary} href={activity.details_url}>
						{display.primary}
					</ActivityLine>
					{display.secondary ? (
						<ActivityLine className={styles.activitySecondary} href={activity.state_url}>
							{display.secondary}
						</ActivityLine>
					) : null}
					<ActivityProgress start={activity.timestamps?.start} end={activity.timestamps?.end} />
					<ActivityTimer start={activity.timestamps?.start} end={activity.timestamps?.end} />
					{activity.buttons?.length ? (
						<div className={styles.activityButtons}>
							{activity.buttons.map((button) => (
								<ExternalLink
									key={`${button.label}:${button.url}`}
									href={button.url}
									className={styles.activityButton}
								>
									{button.label}
								</ExternalLink>
							))}
						</div>
					) : null}
				</div>
			</div>
		</div>
	);
};
