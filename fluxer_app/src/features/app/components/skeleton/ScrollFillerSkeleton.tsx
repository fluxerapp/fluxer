// SPDX-License-Identifier: AGPL-3.0-or-later

import type {PlaceholderMessageGroup, PlaceholderSpecs} from '@app/features/app/components/skeleton/PlaceholderSpecs';
import styles from '@app/features/app/components/skeleton/ScrollFillerSkeleton.module.css';
import {SkeletonBlock} from '@app/features/app/components/skeleton/SkeletonBlock';
import {SkeletonCircle} from '@app/features/app/components/skeleton/SkeletonCircle';
import {SkeletonLine} from '@app/features/app/components/skeleton/SkeletonLine';
import {SkeletonEmphasis, SkeletonRadius} from '@app/features/app/components/skeleton/SkeletonStyle';
import {skeletonSurfaceVar} from '@app/features/app/components/skeleton/SkeletonSurfaceContract';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {flxElementClassName} from '@app/lib/react';
import type React from 'react';

const AVATAR_SIZE = skeletonSurfaceVar('--message-avatar-size');
const COMPACT_AVATAR_SIZE = skeletonSurfaceVar('--message-avatar-size-compact');
const USERNAME_HEIGHT = '0.75rem';
const TIMESTAMP_HEIGHT = '0.625rem';
const LINE_HEIGHT = '0.75rem';
const COMPACT_TIMESTAMP_HEIGHT = '0.625rem';
const COMPACT_USERNAME_HEIGHT = '0.75rem';
const COMPACT_LINE_HEIGHT = '0.75rem';

function resolveSkeletonGroupMarginBottom(
	groupIndex: number,
	groupCount: number,
	groupSpacing: number,
): number | `${number}rem` {
	if (groupIndex === groupCount - 1) {
		return 0;
	}
	return remFromPx(groupSpacing);
}

interface ScrollFillerSkeletonGroupProps {
	readonly group: PlaceholderMessageGroup;
	readonly groupCount: number;
	readonly groupIndex: number;
	readonly groupSpacing: number;
	readonly compactAvatarsVisible: boolean;
}

function CozyScrollFillerSkeletonGroup({
	group,
	groupCount,
	groupIndex,
	groupSpacing,
}: ScrollFillerSkeletonGroupProps): React.ReactElement {
	const firstLineWidth = group.lineWidths[0];
	if (firstLineWidth == null) {
		throw new Error('Cozy message skeleton groups must contain at least one message line.');
	}
	return (
		<flx-message-list-skeleton-group
			className={flxElementClassName(styles.messageGroup)}
			style={{
				marginBottom: resolveSkeletonGroupMarginBottom(groupIndex, groupCount, groupSpacing),
				containIntrinsicSize: `auto ${remFromPx(group.height)}`,
			}}
			data-flx="app.skeleton.scroll-filler-skeleton.cozy-scroll-filler-skeleton-group.message-group"
		>
			<flx-message-list-skeleton-group-row
				className={flxElementClassName(styles.group)}
				data-flx="app.skeleton.scroll-filler-skeleton.cozy-scroll-filler-skeleton-group.group"
			>
				<SkeletonCircle
					size={AVATAR_SIZE}
					className={styles.avatar}
					data-flx="app.skeleton.scroll-filler-skeleton.cozy-scroll-filler-skeleton-group.avatar"
				/>
				<flx-message-list-skeleton-body
					className={flxElementClassName(styles.body)}
					data-flx="app.skeleton.scroll-filler-skeleton.cozy-scroll-filler-skeleton-group.body"
				>
					<flx-message-list-skeleton-header
						className={flxElementClassName(styles.header)}
						data-flx="app.skeleton.scroll-filler-skeleton.cozy-scroll-filler-skeleton-group.header"
					>
						<SkeletonLine
							width={`${group.usernameWidth}%`}
							height={USERNAME_HEIGHT}
							emphasis={SkeletonEmphasis.STRONG}
							data-flx="app.skeleton.scroll-filler-skeleton.cozy-scroll-filler-skeleton-group.skeleton-line"
						/>
						<SkeletonLine
							width={`${group.timestampWidth}%`}
							height={TIMESTAMP_HEIGHT}
							emphasis={SkeletonEmphasis.MUTED}
							data-flx="app.skeleton.scroll-filler-skeleton.cozy-scroll-filler-skeleton-group.skeleton-line--2"
						/>
					</flx-message-list-skeleton-header>
					<flx-message-list-skeleton-lead-line
						className={flxElementClassName(styles.cozyLeadLine)}
						data-flx="app.skeleton.scroll-filler-skeleton.cozy-scroll-filler-skeleton-group.cozy-lead-line"
					>
						<SkeletonLine
							width={`${firstLineWidth}%`}
							height={LINE_HEIGHT}
							data-flx="app.skeleton.scroll-filler-skeleton.cozy-scroll-filler-skeleton-group.skeleton-line--3"
						/>
					</flx-message-list-skeleton-lead-line>
				</flx-message-list-skeleton-body>
			</flx-message-list-skeleton-group-row>
			{group.lineWidths.map((lineWidth, lineIndex) => {
				if (lineIndex === 0) {
					return null;
				}
				return (
					<flx-message-list-skeleton-cozy-continuation
						key={lineIndex}
						className={flxElementClassName(styles.cozyContinuation)}
						data-flx="app.skeleton.scroll-filler-skeleton.cozy-scroll-filler-skeleton-group.cozy-continuation"
					>
						<SkeletonLine
							width={`${lineWidth}%`}
							height={LINE_HEIGHT}
							className={styles.cozyContinuationLine}
							data-flx="app.skeleton.scroll-filler-skeleton.cozy-scroll-filler-skeleton-group.cozy-continuation-line"
						/>
					</flx-message-list-skeleton-cozy-continuation>
				);
			})}
			{group.attachment != null && (
				<flx-message-list-skeleton-cozy-attachment
					className={flxElementClassName(styles.cozyAttachment)}
					data-flx="app.skeleton.scroll-filler-skeleton.cozy-scroll-filler-skeleton-group.cozy-attachment"
				>
					<SkeletonBlock
						width={remFromPx(group.attachment.width)}
						height={remFromPx(group.attachment.height)}
						radius={SkeletonRadius.LARGE}
						emphasis={SkeletonEmphasis.MUTED}
						data-flx="app.skeleton.scroll-filler-skeleton.cozy-scroll-filler-skeleton-group.skeleton-block"
					/>
				</flx-message-list-skeleton-cozy-attachment>
			)}
		</flx-message-list-skeleton-group>
	);
}

function CompactScrollFillerSkeletonGroup({
	group,
	groupCount,
	groupIndex,
	groupSpacing,
	compactAvatarsVisible,
}: ScrollFillerSkeletonGroupProps): React.ReactElement {
	const timestampWidth = `${Math.min(78, 55 + group.timestampWidth)}%`;
	const usernameWidth = `${Math.min(32, group.usernameWidth * 0.45)}%`;
	const usernameLine = (
		<SkeletonLine
			width={usernameWidth}
			height={COMPACT_USERNAME_HEIGHT}
			emphasis={SkeletonEmphasis.STRONG}
			className={styles.compactUsername}
			data-flx="app.skeleton.scroll-filler-skeleton.compact-scroll-filler-skeleton-group.compact-username"
		/>
	);
	const authorPrefix = compactAvatarsVisible ? (
		<flx-message-list-skeleton-compact-author
			className={flxElementClassName(styles.compactAuthorPrefix)}
			data-flx="app.skeleton.scroll-filler-skeleton.compact-scroll-filler-skeleton-group.compact-author-prefix"
		>
			<SkeletonCircle
				size={COMPACT_AVATAR_SIZE}
				className={styles.compactAvatar}
				data-flx="app.skeleton.scroll-filler-skeleton.compact-scroll-filler-skeleton-group.compact-avatar"
			/>
			{usernameLine}
		</flx-message-list-skeleton-compact-author>
	) : (
		usernameLine
	);
	return (
		<flx-message-list-skeleton-group
			className={flxElementClassName(styles.messageGroup)}
			style={{
				marginBottom: resolveSkeletonGroupMarginBottom(groupIndex, groupCount, groupSpacing),
				containIntrinsicSize: `auto ${remFromPx(group.height)}`,
			}}
			data-flx="app.skeleton.scroll-filler-skeleton.compact-scroll-filler-skeleton-group.message-group"
		>
			<flx-message-list-skeleton-compact-messages
				className={flxElementClassName(styles.compactMessages)}
				data-flx="app.skeleton.scroll-filler-skeleton.compact-scroll-filler-skeleton-group.compact-messages"
			>
				{group.lineWidths.map((lineWidth, lineIndex) => (
					<flx-message-list-skeleton-compact-row
						key={lineIndex}
						className={flxElementClassName(styles.compactRow)}
						data-flx="app.skeleton.scroll-filler-skeleton.compact-scroll-filler-skeleton-group.compact-row"
					>
						<flx-message-list-skeleton-compact-timestamp
							className={flxElementClassName(styles.compactTimestamp)}
							data-flx="app.skeleton.scroll-filler-skeleton.compact-scroll-filler-skeleton-group.compact-timestamp"
						>
							<SkeletonLine
								width={timestampWidth}
								height={COMPACT_TIMESTAMP_HEIGHT}
								emphasis={SkeletonEmphasis.MUTED}
								data-flx="app.skeleton.scroll-filler-skeleton.compact-scroll-filler-skeleton-group.skeleton-line"
							/>
						</flx-message-list-skeleton-compact-timestamp>
						<flx-message-list-skeleton-compact-body
							className={flxElementClassName(styles.compactBody)}
							data-flx="app.skeleton.scroll-filler-skeleton.compact-scroll-filler-skeleton-group.compact-body"
						>
							{lineIndex === 0 && authorPrefix}
							<SkeletonLine
								width={`${lineWidth}%`}
								height={COMPACT_LINE_HEIGHT}
								className={styles.compactLine}
								data-flx="app.skeleton.scroll-filler-skeleton.compact-scroll-filler-skeleton-group.compact-line"
							/>
						</flx-message-list-skeleton-compact-body>
					</flx-message-list-skeleton-compact-row>
				))}
			</flx-message-list-skeleton-compact-messages>
			{group.attachment != null && (
				<flx-message-list-skeleton-compact-attachment
					className={flxElementClassName(styles.compactAttachment)}
					data-flx="app.skeleton.scroll-filler-skeleton.compact-scroll-filler-skeleton-group.compact-attachment"
				>
					<SkeletonBlock
						width={remFromPx(group.attachment.width)}
						height={remFromPx(group.attachment.height)}
						radius={SkeletonRadius.LARGE}
						emphasis={SkeletonEmphasis.MUTED}
						data-flx="app.skeleton.scroll-filler-skeleton.compact-scroll-filler-skeleton-group.skeleton-block"
					/>
				</flx-message-list-skeleton-compact-attachment>
			)}
		</flx-message-list-skeleton-group>
	);
}

export const ScrollFillerSkeleton = ({
	compact,
	compactAvatarsVisible,
	groups,
	groupSpacing,
	totalHeight,
}: PlaceholderSpecs) => (
	<flx-message-list-skeleton
		className={flxElementClassName(styles.wrapper, compact && styles.wrapperCompact)}
		style={{minHeight: remFromPx(totalHeight)}}
		data-flx="app.skeleton.scroll-filler-skeleton.wrapper"
	>
		{groups.map((group, groupIndex) => {
			const props = {
				group,
				groupCount: groups.length,
				groupIndex,
				groupSpacing,
				compactAvatarsVisible,
			};
			if (compact) {
				return (
					<CompactScrollFillerSkeletonGroup
						key={groupIndex}
						data-flx="app.skeleton.scroll-filler-skeleton.compact-scroll-filler-skeleton-group"
						{...props}
					/>
				);
			}
			return (
				<CozyScrollFillerSkeletonGroup
					key={groupIndex}
					data-flx="app.skeleton.scroll-filler-skeleton.cozy-scroll-filler-skeleton-group"
					{...props}
				/>
			);
		})}
	</flx-message-list-skeleton>
);
