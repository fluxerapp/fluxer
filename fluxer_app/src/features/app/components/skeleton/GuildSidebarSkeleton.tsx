// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/skeleton/GuildSidebarSkeleton.module.css';
import {SidebarShellSkeleton} from '@app/features/app/components/skeleton/SidebarShellSkeleton';
import {SkeletonBlock} from '@app/features/app/components/skeleton/SkeletonBlock';
import {
	getRememberedSkeletonGuildChannelList,
	getRememberedSkeletonGuildPresentation,
	type RememberedSkeletonGuildChannelList,
	type RememberedSkeletonGuildPresentation,
	SKELETON_UNMEASURED_WIDTH_PX,
	SkeletonGuildBannerPlacement,
} from '@app/features/app/components/skeleton/SkeletonLayoutMemory';
import {SkeletonLine} from '@app/features/app/components/skeleton/SkeletonLine';
import {createSkeletonRandomFromKey} from '@app/features/app/components/skeleton/SkeletonSeed';
import {SkeletonEmphasis, SkeletonRadius} from '@app/features/app/components/skeleton/SkeletonStyle';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {flxElementClassName} from '@app/lib/react';
import type React from 'react';
import {useMemo} from 'react';

const FALLBACK_CATEGORY_CHANNEL_COUNTS: ReadonlyArray<number> = [3, 5, 4, 6];
const CATEGORY_CARET_SIZE = '0.75rem';
const CATEGORY_LABEL_HEIGHT = '0.625rem';
const CATEGORY_LABEL_WIDTH_MIN = 28;
const CATEGORY_LABEL_WIDTH_RANGE = 24;
const CHANNEL_ICON_SIZE = '1.25rem';
const CHANNEL_LABEL_HEIGHT = '0.75rem';
const CHANNEL_LABEL_WIDTH_MIN = 34;
const CHANNEL_LABEL_WIDTH_RANGE = 40;
const MEMBERS_LABEL_WIDTH = '4rem';

interface GuildChannelSpec {
	readonly labelWidth: string;
}

interface GuildCategorySpec {
	readonly headerVisible: boolean;
	readonly labelWidth: string;
	readonly channels: ReadonlyArray<GuildChannelSpec>;
}

function resolveMeasuredWidth(widthPx: number, fallbackWidth: string): string {
	if (widthPx === SKELETON_UNMEASURED_WIDTH_PX) {
		return fallbackWidth;
	}
	return remFromPx(widthPx);
}

function createFallbackCategorySpecs(): ReadonlyArray<GuildCategorySpec> {
	const random = createSkeletonRandomFromKey('guild-sidebar-skeleton-channels');
	return FALLBACK_CATEGORY_CHANNEL_COUNTS.map((channelCount) => ({
		headerVisible: true,
		labelWidth: `${CATEGORY_LABEL_WIDTH_MIN + random() * CATEGORY_LABEL_WIDTH_RANGE}%`,
		channels: Array.from({length: channelCount}, () => ({
			labelWidth: `${CHANNEL_LABEL_WIDTH_MIN + random() * CHANNEL_LABEL_WIDTH_RANGE}%`,
		})),
	}));
}

function createRememberedCategorySpecs(
	channelList: RememberedSkeletonGuildChannelList,
): ReadonlyArray<GuildCategorySpec> {
	const random = createSkeletonRandomFromKey('guild-sidebar-skeleton-channels');
	return channelList.groups.map((group) => ({
		headerVisible: group.categoryHeaderVisible,
		labelWidth: resolveMeasuredWidth(
			group.categoryNameWidthPx,
			`${CATEGORY_LABEL_WIDTH_MIN + random() * CATEGORY_LABEL_WIDTH_RANGE}%`,
		),
		channels: group.channels.map((channel) => ({
			labelWidth: resolveMeasuredWidth(
				channel.nameWidthPx,
				`${CHANNEL_LABEL_WIDTH_MIN + random() * CHANNEL_LABEL_WIDTH_RANGE}%`,
			),
		})),
	}));
}

function GuildChannelSkeletonRow({width}: {readonly width: string}) {
	return (
		<flx-app-guild-sidebar-skeleton-channel
			className={flxElementClassName(styles.row, styles.rowChannel)}
			data-flx="app.skeleton.guild-sidebar-skeleton.guild-channel-skeleton-row.row"
		>
			<SkeletonBlock
				width={CHANNEL_ICON_SIZE}
				height={CHANNEL_ICON_SIZE}
				radius={SkeletonRadius.SMALL}
				emphasis={SkeletonEmphasis.MUTED}
				className={styles.channelIcon}
				data-flx="app.skeleton.guild-sidebar-skeleton.guild-channel-skeleton-row.channel-icon"
			/>
			<SkeletonLine
				width={width}
				height={CHANNEL_LABEL_HEIGHT}
				data-flx="app.skeleton.guild-sidebar-skeleton.guild-channel-skeleton-row.skeleton-line"
			/>
		</flx-app-guild-sidebar-skeleton-channel>
	);
}

function GuildCategorySkeletonGroup({category}: {readonly category: GuildCategorySpec}) {
	return (
		<flx-app-guild-sidebar-skeleton-group
			className={flxElementClassName(styles.group)}
			data-flx="app.skeleton.guild-sidebar-skeleton.guild-category-skeleton-group.group"
		>
			{category.headerVisible && (
				<flx-app-guild-sidebar-skeleton-category
					className={flxElementClassName(styles.row, styles.rowCategory)}
					data-flx="app.skeleton.guild-sidebar-skeleton.guild-category-skeleton-group.row"
				>
					<SkeletonLine
						width={category.labelWidth}
						height={CATEGORY_LABEL_HEIGHT}
						emphasis={SkeletonEmphasis.MUTED}
						data-flx="app.skeleton.guild-sidebar-skeleton.guild-category-skeleton-group.skeleton-line"
					/>
					<SkeletonBlock
						width={CATEGORY_CARET_SIZE}
						height={CATEGORY_CARET_SIZE}
						radius={SkeletonRadius.SMALL}
						emphasis={SkeletonEmphasis.MUTED}
						className={styles.categoryCaret}
						data-flx="app.skeleton.guild-sidebar-skeleton.guild-category-skeleton-group.category-caret"
					/>
				</flx-app-guild-sidebar-skeleton-category>
			)}
			{category.channels.map((channel, channelIndex) => (
				<GuildChannelSkeletonRow
					key={channelIndex}
					width={channel.labelWidth}
					data-flx="app.skeleton.guild-sidebar-skeleton.guild-category-skeleton-group.guild-channel-skeleton-row"
				/>
			))}
		</flx-app-guild-sidebar-skeleton-group>
	);
}

interface GuildChannelListSkeletonProps {
	readonly channelList: RememberedSkeletonGuildChannelList | null;
	readonly detachedBannerAspectRatio: number | null;
}

export const GuildChannelListSkeleton: React.FC<GuildChannelListSkeletonProps> = ({
	channelList,
	detachedBannerAspectRatio,
}) => {
	const categories = useMemo<ReadonlyArray<GuildCategorySpec>>(() => {
		if (channelList == null) {
			return createFallbackCategorySpecs();
		}
		return createRememberedCategorySpecs(channelList);
	}, [channelList]);
	const membersRowVisible = channelList?.membersRowVisible ?? false;
	let detachedBannerStyle: React.CSSProperties | undefined;
	if (detachedBannerAspectRatio != null) {
		detachedBannerStyle = {aspectRatio: `${detachedBannerAspectRatio}`};
	}
	return (
		<flx-app-guild-sidebar-skeleton-navigation
			className={flxElementClassName(styles.navigation)}
			aria-hidden
			data-flx="app.skeleton.guild-sidebar-skeleton.guild-channel-list-skeleton.navigation"
		>
			{detachedBannerStyle != null && (
				<flx-app-guild-sidebar-skeleton-detached-banner
					className={flxElementClassName(styles.detachedBanner)}
					style={detachedBannerStyle}
					data-flx="app.skeleton.guild-sidebar-skeleton.guild-channel-list-skeleton.detached-banner"
				>
					<SkeletonBlock
						width="100%"
						height="100%"
						radius={SkeletonRadius.SHARP}
						emphasis={SkeletonEmphasis.MUTED}
						data-flx="app.skeleton.guild-sidebar-skeleton.guild-channel-list-skeleton.skeleton-block"
					/>
				</flx-app-guild-sidebar-skeleton-detached-banner>
			)}
			<flx-app-guild-sidebar-skeleton-top-spacer
				className={flxElementClassName(styles.topSpacer)}
				data-flx="app.skeleton.guild-sidebar-skeleton.guild-channel-list-skeleton.top-spacer"
			/>
			{membersRowVisible && (
				<>
					<flx-app-guild-sidebar-skeleton-members
						className={flxElementClassName(styles.membersSection)}
						data-flx="app.skeleton.guild-sidebar-skeleton.guild-channel-list-skeleton.members-section"
					>
						<flx-app-guild-sidebar-skeleton-members-row
							className={flxElementClassName(styles.row, styles.rowChannel, styles.rowMembers)}
							data-flx="app.skeleton.guild-sidebar-skeleton.guild-channel-list-skeleton.row"
						>
							<SkeletonBlock
								width={CHANNEL_ICON_SIZE}
								height={CHANNEL_ICON_SIZE}
								radius={SkeletonRadius.SMALL}
								emphasis={SkeletonEmphasis.MUTED}
								className={styles.channelIcon}
								data-flx="app.skeleton.guild-sidebar-skeleton.guild-channel-list-skeleton.channel-icon"
							/>
							<SkeletonLine
								width={MEMBERS_LABEL_WIDTH}
								height={CHANNEL_LABEL_HEIGHT}
								data-flx="app.skeleton.guild-sidebar-skeleton.guild-channel-list-skeleton.skeleton-line"
							/>
						</flx-app-guild-sidebar-skeleton-members-row>
					</flx-app-guild-sidebar-skeleton-members>
					<flx-app-guild-sidebar-skeleton-members-separator
						className={flxElementClassName(styles.membersSeparator)}
						data-flx="app.skeleton.guild-sidebar-skeleton.guild-channel-list-skeleton.members-separator"
					/>
				</>
			)}
			<flx-app-guild-sidebar-skeleton-groups
				className={flxElementClassName(styles.groups)}
				data-flx="app.skeleton.guild-sidebar-skeleton.guild-channel-list-skeleton.groups"
			>
				{categories.map((category, categoryIndex) => (
					<GuildCategorySkeletonGroup
						key={categoryIndex}
						category={category}
						data-flx="app.skeleton.guild-sidebar-skeleton.guild-channel-list-skeleton.guild-category-skeleton-group"
					/>
				))}
			</flx-app-guild-sidebar-skeleton-groups>
			{categories.length > 0 && (
				<flx-app-guild-sidebar-skeleton-bottom-drop-zone
					className={flxElementClassName(styles.bottomDropZone)}
					data-flx="app.skeleton.guild-sidebar-skeleton.guild-channel-list-skeleton.bottom-drop-zone"
				/>
			)}
		</flx-app-guild-sidebar-skeleton-navigation>
	);
};

interface GuildSidebarSkeletonProps {
	readonly guildId: string | null;
}

function resolveDetachedBannerAspectRatio(presentation: RememberedSkeletonGuildPresentation | null): number | null {
	if (presentation == null || presentation.bannerPlacement !== SkeletonGuildBannerPlacement.DETACHED) {
		return null;
	}
	return presentation.bannerAspectRatio;
}

export const GuildSidebarSkeleton: React.FC<GuildSidebarSkeletonProps> = ({guildId}) => {
	const remembered = useMemo(() => {
		if (guildId == null) {
			return Object.freeze({
				presentation: null as RememberedSkeletonGuildPresentation | null,
				channelList: null as RememberedSkeletonGuildChannelList | null,
			});
		}
		return Object.freeze({
			presentation: getRememberedSkeletonGuildPresentation(guildId),
			channelList: getRememberedSkeletonGuildChannelList(guildId),
		});
	}, [guildId]);
	return (
		<SidebarShellSkeleton
			guildPresentation={remembered.presentation}
			data-flx="app.skeleton.guild-sidebar-skeleton.sidebar-shell-skeleton"
		>
			<GuildChannelListSkeleton
				channelList={remembered.channelList}
				detachedBannerAspectRatio={resolveDetachedBannerAspectRatio(remembered.presentation)}
				data-flx="app.skeleton.guild-sidebar-skeleton.guild-channel-list-skeleton"
			/>
			<flx-app-guild-sidebar-skeleton-bottom-spacer
				className={flxElementClassName(styles.bottomSpacer)}
				data-flx="app.skeleton.guild-sidebar-skeleton.bottom-spacer"
			/>
		</SidebarShellSkeleton>
	);
};
