// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/skeleton/FriendsSkeleton.module.css';
import {SkeletonBlock} from '@app/features/app/components/skeleton/SkeletonBlock';
import {SkeletonCircle} from '@app/features/app/components/skeleton/SkeletonCircle';
import {
	getRememberedSkeletonChannelHeaderLayout,
	getRememberedSkeletonFriendsLayout,
	type RememberedSkeletonActiveNowCard,
	type RememberedSkeletonFriendsLayout,
	SKELETON_DEFAULT_CHANNEL_HEADER_LAYOUT,
	SKELETON_DEFAULT_FRIENDS_LAYOUT,
	SKELETON_UNMEASURED_WIDTH_PX,
	SkeletonFriendsTab,
} from '@app/features/app/components/skeleton/SkeletonLayoutMemory';
import {SkeletonLine} from '@app/features/app/components/skeleton/SkeletonLine';
import {createSkeletonRandomFromKey} from '@app/features/app/components/skeleton/SkeletonSeed';
import {SkeletonEmphasis, SkeletonRadius} from '@app/features/app/components/skeleton/SkeletonStyle';
import type {SkeletonInjectedToken} from '@app/features/app/components/skeleton/SkeletonSurfaceContract';
import Initialization from '@app/features/app/state/Initialization';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {resolveAvatarStackGeometry, resolveAvatarStackWidthRem} from '@app/features/ui/avatars/AvatarStackGeometry';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {
	VOICE_ACTIVITY_AVATAR_MAX_VISIBLE,
	VOICE_ACTIVITY_AVATAR_SIZE_PX,
} from '@app/features/user/components/profile/VoiceActivityCardMetrics';
import PrivacyPreferences from '@app/features/user/state/PrivacyPreferences';
import {flxElementClassName} from '@app/lib/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {type CSSProperties, useMemo, useState} from 'react';

const FALLBACK_TAB_WIDTHS: ReadonlyArray<string> = ['4.5rem', '2.75rem', '5rem', '6.25rem'];
const PENDING_TAB_INDEX = 2;
const PENDING_BADGE_ALLOWANCE = '1.75rem';
const TAB_HEIGHT = '2rem';
const BACK_BUTTON_SIZE = '2rem';
const TITLE_ICON_SIZE = '1.5rem';
const TITLE_WIDTH = '5.5rem';
const TITLE_HEIGHT = '0.875rem';
const HEADER_ACTION_SIZE = '2rem';
const SEARCH_HEIGHT = '2.75rem';
const SECTION_TITLE_WIDTH = '7rem';
const SECTION_TITLE_HEIGHT = '0.625rem';
const EMPTY_STATE_ICON_SIZE = '4rem';
const EMPTY_STATE_TITLE_WIDTH = '18rem';
const EMPTY_STATE_TITLE_HEIGHT = '1.125rem';
const EMPTY_STATE_SUBTITLE_WIDTH = '22rem';
const EMPTY_STATE_SUBTITLE_HEIGHT = '0.75rem';
const ROW_AVATAR_SIZE = '2.25rem';
const ROW_ACTION_SIZE = '2.25rem';
const ROW_NAME_HEIGHT = '0.75rem';
const ROW_SUBTEXT_HEIGHT = '0.625rem';
const ROW_ACTION_COUNT = 2;
const OUTGOING_ROW_ACTION_COUNT = 1;
const ROW_HEIGHT_PX = 60;
const MAX_FALLBACK_ROW_COUNT = 24;
const ROW_NAME_WIDTH_MIN = 28;
const ROW_NAME_WIDTH_RANGE = 34;
const ROW_SUBTEXT_WIDTH_MIN = 18;
const ROW_SUBTEXT_WIDTH_RANGE = 26;
const SIDEBAR_TITLE_WIDTH = '6.5rem';
const SIDEBAR_TITLE_HEIGHT = '1.125rem';
const SIDEBAR_EMPTY_ICON_SIZE = '3rem';
const SIDEBAR_EMPTY_TITLE_WIDTH = '9rem';
const SIDEBAR_EMPTY_TITLE_HEIGHT = '0.8125rem';
const SIDEBAR_EMPTY_DESCRIPTION_HEIGHT = '0.6875rem';
const SIDEBAR_EMPTY_DESCRIPTION_WIDTHS: ReadonlyArray<string> = ['100%', '68%'];
const ACTIVE_NOW_LABEL_HEIGHT = '0.625rem';
const ACTIVE_NOW_IN_VOICE_LABEL_WIDTH = '3.75rem';
const ACTIVE_NOW_STREAMING_LABEL_WIDTH = '4.75rem';
const ACTIVE_NOW_CONTEXT_HEIGHT = '0.625rem';
const ACTIVE_NOW_CONTEXT_WIDTH_MIN = 6;
const ACTIVE_NOW_CONTEXT_WIDTH_RANGE = 5;
const ACTIVE_NOW_PARTICIPANT_GEOMETRY = resolveAvatarStackGeometry(VOICE_ACTIVITY_AVATAR_SIZE_PX);
const ACTIVE_NOW_PARTICIPANT_METRICS = {
	'--active-now-participant-size': ACTIVE_NOW_PARTICIPANT_GEOMETRY.sizeRem,
	'--active-now-participant-overlap': ACTIVE_NOW_PARTICIPANT_GEOMETRY.overlapRem,
	'--active-now-participant-outline': ACTIVE_NOW_PARTICIPANT_GEOMETRY.outlineRem,
} satisfies Partial<Record<SkeletonInjectedToken, string>>;
const ACTIVE_NOW_PARTICIPANT_STYLE = ACTIVE_NOW_PARTICIPANT_METRICS as CSSProperties;
const ACTIVE_NOW_ACTION_ICON_SIZE = '1rem';
const ACTIVE_NOW_ACTION_LABEL_WIDTH = '4.75rem';
const ACTIVE_NOW_ACTION_LABEL_HEIGHT = '0.625rem';

interface FriendRowSpec {
	readonly nameWidth: string;
	readonly subtextWidth: string;
}

interface ActiveNowCardSpec {
	readonly contextWidth: string;
	readonly participantCount: number;
	readonly streaming: boolean;
}

const FALLBACK_ACTIVE_NOW_CARDS: ReadonlyArray<RememberedSkeletonActiveNowCard> = Object.freeze([
	Object.freeze({participantCount: 3, streaming: false}),
	Object.freeze({participantCount: 2, streaming: false}),
	Object.freeze({participantCount: 4, streaming: false}),
]);

function resolveViewportRowCount(): number {
	if (typeof window === 'undefined') {
		return MAX_FALLBACK_ROW_COUNT;
	}
	return Math.min(MAX_FALLBACK_ROW_COUNT, Math.max(1, Math.ceil(window.innerHeight / ROW_HEIGHT_PX)));
}

function resolveTabRowCount(layout: RememberedSkeletonFriendsLayout): number {
	if (layout.activeTab === SkeletonFriendsTab.ONLINE) {
		return layout.onlineRowCount;
	}
	if (layout.activeTab === SkeletonFriendsTab.ALL) {
		return layout.allRowCount;
	}
	if (layout.activeTab === SkeletonFriendsTab.PENDING) {
		return layout.pendingRowCount;
	}
	return 0;
}

function resolveTabWidths(layout: RememberedSkeletonFriendsLayout): ReadonlyArray<string> {
	return FALLBACK_TAB_WIDTHS.map((fallbackWidth, index) => {
		const measuredWidthPx = layout.tabWidthsPx[index] ?? SKELETON_UNMEASURED_WIDTH_PX;
		if (measuredWidthPx !== SKELETON_UNMEASURED_WIDTH_PX) {
			return remFromPx(measuredWidthPx);
		}
		if (index === PENDING_TAB_INDEX && layout.pendingBadgeVisible) {
			return `calc(${fallbackWidth} + ${PENDING_BADGE_ALLOWANCE})`;
		}
		return fallbackWidth;
	});
}

function resolveParticipantsWidth(participantCount: number): string {
	return resolveAvatarStackWidthRem(participantCount, VOICE_ACTIVITY_AVATAR_SIZE_PX, VOICE_ACTIVITY_AVATAR_MAX_VISIBLE);
}

function createActiveNowCardSpecs(
	cards: ReadonlyArray<RememberedSkeletonActiveNowCard>,
): ReadonlyArray<ActiveNowCardSpec> {
	const random = createSkeletonRandomFromKey('friends-skeleton-active-now');
	return cards.map((card) => ({
		contextWidth: `${ACTIVE_NOW_CONTEXT_WIDTH_MIN + random() * ACTIVE_NOW_CONTEXT_WIDTH_RANGE}rem`,
		participantCount: card.participantCount,
		streaming: card.streaming,
	}));
}

function FriendSkeletonRow({row, actionCount}: {readonly row: FriendRowSpec; readonly actionCount: number}) {
	return (
		<flx-app-friends-skeleton-row
			className={flxElementClassName(styles.row)}
			data-flx="app.skeleton.friends-skeleton.friend-skeleton-row.row"
		>
			<flx-app-friends-skeleton-row-info
				className={flxElementClassName(styles.rowInfo)}
				data-flx="app.skeleton.friends-skeleton.friend-skeleton-row.row-info"
			>
				<SkeletonCircle
					size={ROW_AVATAR_SIZE}
					data-flx="app.skeleton.friends-skeleton.friend-skeleton-row.skeleton-circle"
				/>
				<flx-app-friends-skeleton-row-details
					className={flxElementClassName(styles.rowDetails)}
					data-flx="app.skeleton.friends-skeleton.friend-skeleton-row.row-details"
				>
					<flx-app-friends-skeleton-row-name
						className={flxElementClassName(styles.rowNameBand)}
						data-flx="app.skeleton.friends-skeleton.friend-skeleton-row.row-name-band"
					>
						<SkeletonLine
							width={row.nameWidth}
							height={ROW_NAME_HEIGHT}
							emphasis={SkeletonEmphasis.STRONG}
							data-flx="app.skeleton.friends-skeleton.friend-skeleton-row.skeleton-line"
						/>
					</flx-app-friends-skeleton-row-name>
					<flx-app-friends-skeleton-row-subtext
						className={flxElementClassName(styles.rowSubtextBand)}
						data-flx="app.skeleton.friends-skeleton.friend-skeleton-row.row-subtext-band"
					>
						<SkeletonLine
							width={row.subtextWidth}
							height={ROW_SUBTEXT_HEIGHT}
							emphasis={SkeletonEmphasis.MUTED}
							data-flx="app.skeleton.friends-skeleton.friend-skeleton-row.skeleton-line--2"
						/>
					</flx-app-friends-skeleton-row-subtext>
				</flx-app-friends-skeleton-row-details>
			</flx-app-friends-skeleton-row-info>
			<flx-app-friends-skeleton-row-actions
				className={flxElementClassName(styles.rowActions)}
				data-flx="app.skeleton.friends-skeleton.friend-skeleton-row.row-actions"
			>
				{Array.from({length: actionCount}, (_unused, actionIndex) => (
					<SkeletonCircle
						key={actionIndex}
						size={ROW_ACTION_SIZE}
						emphasis={SkeletonEmphasis.MUTED}
						data-flx="app.skeleton.friends-skeleton.friend-skeleton-row.skeleton-circle--2"
					/>
				))}
			</flx-app-friends-skeleton-row-actions>
		</flx-app-friends-skeleton-row>
	);
}

interface FriendsListSectionSpec {
	readonly rows: ReadonlyArray<FriendRowSpec>;
	readonly actionCount: number;
	readonly withMargin: boolean;
}

function resolveFriendsListSections(
	layout: RememberedSkeletonFriendsLayout,
	rows: ReadonlyArray<FriendRowSpec>,
): ReadonlyArray<FriendsListSectionSpec> {
	if (layout.activeTab !== SkeletonFriendsTab.PENDING) {
		return [{rows, actionCount: ROW_ACTION_COUNT, withMargin: false}];
	}
	const incomingRowCount = Math.min(layout.pendingIncomingRowCount, rows.length);
	const incomingRows = rows.slice(0, incomingRowCount);
	const outgoingRows = rows.slice(incomingRowCount);
	if (incomingRows.length === 0) {
		return [{rows: outgoingRows, actionCount: OUTGOING_ROW_ACTION_COUNT, withMargin: false}];
	}
	if (outgoingRows.length === 0) {
		return [{rows: incomingRows, actionCount: ROW_ACTION_COUNT, withMargin: true}];
	}
	return [
		{rows: incomingRows, actionCount: ROW_ACTION_COUNT, withMargin: true},
		{rows: outgoingRows, actionCount: OUTGOING_ROW_ACTION_COUNT, withMargin: false},
	];
}

interface FriendsListSectionSkeletonProps {
	readonly rows: ReadonlyArray<FriendRowSpec>;
	readonly actionCount: number;
	readonly withMargin: boolean;
}

function FriendsListSectionSkeleton({rows, actionCount, withMargin}: FriendsListSectionSkeletonProps) {
	return (
		<>
			<flx-app-friends-skeleton-section-title
				className={flxElementClassName(styles.sectionTitle)}
				data-flx="app.skeleton.friends-skeleton.friends-list-section-skeleton.section-title"
			>
				<SkeletonLine
					width={SECTION_TITLE_WIDTH}
					height={SECTION_TITLE_HEIGHT}
					emphasis={SkeletonEmphasis.MUTED}
					data-flx="app.skeleton.friends-skeleton.friends-list-section-skeleton.skeleton-line"
				/>
			</flx-app-friends-skeleton-section-title>
			<flx-app-friends-skeleton-rows
				className={flxElementClassName(withMargin ? styles.rowsWithMargin : styles.rows)}
				data-flx="app.skeleton.friends-skeleton.friends-list-section-skeleton.rows-with-margin"
			>
				{rows.map((row, index) => (
					<FriendSkeletonRow
						key={index}
						row={row}
						actionCount={actionCount}
						data-flx="app.skeleton.friends-skeleton.friends-list-section-skeleton.friend-skeleton-row"
					/>
				))}
			</flx-app-friends-skeleton-rows>
		</>
	);
}

function FriendsListEmptyStateSkeleton() {
	return (
		<flx-app-friends-skeleton-empty
			className={flxElementClassName(styles.emptyState)}
			data-flx="app.skeleton.friends-skeleton.friends-list-empty-state-skeleton.empty-state"
		>
			<SkeletonBlock
				width={EMPTY_STATE_ICON_SIZE}
				height={EMPTY_STATE_ICON_SIZE}
				radius={SkeletonRadius.MEDIUM}
				emphasis={SkeletonEmphasis.MUTED}
				className={styles.emptyStateIcon}
				data-flx="app.skeleton.friends-skeleton.friends-list-empty-state-skeleton.empty-state-icon"
			/>
			<flx-app-friends-skeleton-empty-title
				className={flxElementClassName(styles.emptyStateTitle)}
				data-flx="app.skeleton.friends-skeleton.friends-list-empty-state-skeleton.empty-state-title"
			>
				<SkeletonLine
					width={EMPTY_STATE_TITLE_WIDTH}
					height={EMPTY_STATE_TITLE_HEIGHT}
					emphasis={SkeletonEmphasis.STRONG}
					data-flx="app.skeleton.friends-skeleton.friends-list-empty-state-skeleton.skeleton-line"
				/>
			</flx-app-friends-skeleton-empty-title>
			<flx-app-friends-skeleton-empty-subtitle
				className={flxElementClassName(styles.emptyStateSubtitle)}
				data-flx="app.skeleton.friends-skeleton.friends-list-empty-state-skeleton.empty-state-subtitle"
			>
				<SkeletonLine
					width={EMPTY_STATE_SUBTITLE_WIDTH}
					height={EMPTY_STATE_SUBTITLE_HEIGHT}
					emphasis={SkeletonEmphasis.MUTED}
					data-flx="app.skeleton.friends-skeleton.friends-list-empty-state-skeleton.skeleton-line--2"
				/>
			</flx-app-friends-skeleton-empty-subtitle>
		</flx-app-friends-skeleton-empty>
	);
}

function ActiveNowEmptyStateSkeleton() {
	return (
		<flx-app-friends-skeleton-active-empty
			className={flxElementClassName(styles.sidebarEmptyState)}
			data-flx="app.skeleton.friends-skeleton.active-now-empty-state-skeleton.sidebar-empty-state"
		>
			<SkeletonBlock
				width={SIDEBAR_EMPTY_ICON_SIZE}
				height={SIDEBAR_EMPTY_ICON_SIZE}
				radius={SkeletonRadius.MEDIUM}
				emphasis={SkeletonEmphasis.MUTED}
				data-flx="app.skeleton.friends-skeleton.active-now-empty-state-skeleton.skeleton-block"
			/>
			<flx-app-friends-skeleton-active-empty-title
				className={flxElementClassName(styles.sidebarEmptyTitle)}
				data-flx="app.skeleton.friends-skeleton.active-now-empty-state-skeleton.sidebar-empty-title"
			>
				<SkeletonLine
					width={SIDEBAR_EMPTY_TITLE_WIDTH}
					height={SIDEBAR_EMPTY_TITLE_HEIGHT}
					emphasis={SkeletonEmphasis.STRONG}
					data-flx="app.skeleton.friends-skeleton.active-now-empty-state-skeleton.skeleton-line"
				/>
			</flx-app-friends-skeleton-active-empty-title>
			<flx-app-friends-skeleton-active-empty-description
				className={flxElementClassName(styles.sidebarEmptyDescription)}
				data-flx="app.skeleton.friends-skeleton.active-now-empty-state-skeleton.sidebar-empty-description"
			>
				{SIDEBAR_EMPTY_DESCRIPTION_WIDTHS.map((lineWidth) => (
					<flx-app-friends-skeleton-active-empty-description-line
						key={lineWidth}
						className={flxElementClassName(styles.sidebarEmptyDescriptionLine)}
						data-flx="app.skeleton.friends-skeleton.active-now-empty-state-skeleton.sidebar-empty-description-line"
					>
						<SkeletonLine
							width={lineWidth}
							height={SIDEBAR_EMPTY_DESCRIPTION_HEIGHT}
							emphasis={SkeletonEmphasis.MUTED}
							data-flx="app.skeleton.friends-skeleton.active-now-empty-state-skeleton.skeleton-line--2"
						/>
					</flx-app-friends-skeleton-active-empty-description-line>
				))}
			</flx-app-friends-skeleton-active-empty-description>
		</flx-app-friends-skeleton-active-empty>
	);
}

function ActiveNowCardSkeleton({spec}: {readonly spec: ActiveNowCardSpec}) {
	let labelWidth = ACTIVE_NOW_IN_VOICE_LABEL_WIDTH;
	if (spec.streaming) {
		labelWidth = ACTIVE_NOW_STREAMING_LABEL_WIDTH;
	}
	return (
		<flx-app-friends-skeleton-active-card
			className={flxElementClassName(styles.activeNowCard)}
			style={ACTIVE_NOW_PARTICIPANT_STYLE}
			data-flx="app.skeleton.friends-skeleton.active-now-card-skeleton.active-now-card"
		>
			<flx-app-friends-skeleton-active-context-group
				className={flxElementClassName(styles.activeNowContextGroup)}
				data-flx="app.skeleton.friends-skeleton.active-now-card-skeleton.active-now-context-group"
			>
				<flx-app-friends-skeleton-active-status
					className={flxElementClassName(styles.activeNowStatus)}
					data-flx="app.skeleton.friends-skeleton.active-now-card-skeleton.active-now-status"
				>
					<SkeletonLine
						width={labelWidth}
						height={ACTIVE_NOW_LABEL_HEIGHT}
						emphasis={SkeletonEmphasis.STRONG}
						data-flx="app.skeleton.friends-skeleton.active-now-card-skeleton.skeleton-line"
					/>
				</flx-app-friends-skeleton-active-status>
				<flx-app-friends-skeleton-active-context
					className={flxElementClassName(styles.activeNowContext)}
					data-flx="app.skeleton.friends-skeleton.active-now-card-skeleton.active-now-context"
				>
					<SkeletonLine
						width={spec.contextWidth}
						height={ACTIVE_NOW_CONTEXT_HEIGHT}
						emphasis={SkeletonEmphasis.MUTED}
						data-flx="app.skeleton.friends-skeleton.active-now-card-skeleton.skeleton-line--2"
					/>
				</flx-app-friends-skeleton-active-context>
			</flx-app-friends-skeleton-active-context-group>
			{spec.participantCount > 0 && (
				<flx-app-friends-skeleton-active-participants
					className={flxElementClassName(styles.activeNowParticipants)}
					data-flx="app.skeleton.friends-skeleton.active-now-card-skeleton.active-now-participants"
				>
					<SkeletonBlock
						width={resolveParticipantsWidth(spec.participantCount)}
						height={ACTIVE_NOW_PARTICIPANT_GEOMETRY.sizeRem}
						radius={SkeletonRadius.PILL}
						emphasis={SkeletonEmphasis.MUTED}
						data-flx="app.skeleton.friends-skeleton.active-now-card-skeleton.skeleton-block"
					/>
				</flx-app-friends-skeleton-active-participants>
			)}
			<flx-app-friends-skeleton-active-action
				className={flxElementClassName(styles.activeNowAction)}
				data-flx="app.skeleton.friends-skeleton.active-now-card-skeleton.active-now-action"
			>
				<SkeletonBlock
					width={ACTIVE_NOW_ACTION_ICON_SIZE}
					height={ACTIVE_NOW_ACTION_ICON_SIZE}
					radius={SkeletonRadius.SMALL}
					emphasis={SkeletonEmphasis.MUTED}
					data-flx="app.skeleton.friends-skeleton.active-now-card-skeleton.skeleton-block--2"
				/>
				<SkeletonLine
					width={ACTIVE_NOW_ACTION_LABEL_WIDTH}
					height={ACTIVE_NOW_ACTION_LABEL_HEIGHT}
					emphasis={SkeletonEmphasis.MUTED}
					data-flx="app.skeleton.friends-skeleton.active-now-card-skeleton.skeleton-line--3"
				/>
			</flx-app-friends-skeleton-active-action>
		</flx-app-friends-skeleton-active-card>
	);
}

export const FriendsSkeleton = observer(function FriendsSkeleton() {
	const [{activeNowCards, headerLayout, layout, rowCount, showActiveNow}] = useState(() => {
		const rememberedLayout = getRememberedSkeletonFriendsLayout();
		const layout = rememberedLayout ?? SKELETON_DEFAULT_FRIENDS_LAYOUT;
		return {
			activeNowCards: rememberedLayout != null ? rememberedLayout.activeNowCards : FALLBACK_ACTIVE_NOW_CARDS,
			headerLayout: getRememberedSkeletonChannelHeaderLayout() ?? SKELETON_DEFAULT_CHANNEL_HEADER_LAYOUT,
			layout,
			rowCount: rememberedLayout != null ? resolveTabRowCount(rememberedLayout) : resolveViewportRowCount(),
			showActiveNow: Initialization.isReady ? PrivacyPreferences.getShowActiveNow() : layout.activeNowVisible,
		};
	});
	const isMobile = MobileLayout.enabled;
	const tabWidths = useMemo(() => resolveTabWidths(layout), [layout]);
	const showSearchBar = layout.activeTab !== SkeletonFriendsTab.ADD;
	const headerActionCount = useMemo(() => {
		if (isMobile) {
			return 0;
		}
		let count = 1;
		if (headerLayout.updaterVisible) {
			count += 1;
		}
		if (headerLayout.staffToolsVisible) {
			count += 1;
		}
		return count;
	}, [headerLayout, isMobile]);
	const rows = useMemo<ReadonlyArray<FriendRowSpec>>(() => {
		const random = createSkeletonRandomFromKey('friends-skeleton-rows');
		return Array.from({length: rowCount}, () => ({
			nameWidth: `${ROW_NAME_WIDTH_MIN + random() * ROW_NAME_WIDTH_RANGE}%`,
			subtextWidth: `${ROW_SUBTEXT_WIDTH_MIN + random() * ROW_SUBTEXT_WIDTH_RANGE}%`,
		}));
	}, [rowCount]);
	const listSections = useMemo(() => resolveFriendsListSections(layout, rows), [layout, rows]);
	const activeNowCardSpecs = useMemo(() => createActiveNowCardSpecs(activeNowCards), [activeNowCards]);
	return (
		<flx-app-friends-skeleton
			className={flxElementClassName(styles.container)}
			aria-hidden
			data-flx="app.skeleton.friends-skeleton.container"
		>
			<flx-app-friends-skeleton-main
				className={flxElementClassName(styles.mainColumn)}
				data-flx="app.skeleton.friends-skeleton.main-column"
			>
				<flx-app-friends-skeleton-header
					className={flxElementClassName(styles.header)}
					data-flx="app.skeleton.friends-skeleton.header"
				>
					<flx-app-friends-skeleton-header-left-section
						className={flxElementClassName(styles.headerLeftSection)}
						data-flx="app.skeleton.friends-skeleton.header-left-section"
					>
						<SkeletonCircle
							size={BACK_BUTTON_SIZE}
							emphasis={SkeletonEmphasis.MUTED}
							className={clsx(styles.backButton, isMobile && styles.backButtonMobile)}
							data-flx="app.skeleton.friends-skeleton.back-button"
						/>
						<flx-app-friends-skeleton-header-left
							className={flxElementClassName(styles.headerLeft)}
							data-flx="app.skeleton.friends-skeleton.header-left"
						>
							{!isMobile && (
								<>
									<flx-app-friends-skeleton-title
										className={flxElementClassName(styles.titleSection)}
										data-flx="app.skeleton.friends-skeleton.title-section"
									>
										<SkeletonBlock
											width={TITLE_ICON_SIZE}
											height={TITLE_ICON_SIZE}
											radius={SkeletonRadius.SMALL}
											emphasis={SkeletonEmphasis.STRONG}
											data-flx="app.skeleton.friends-skeleton.skeleton-block"
										/>
										<SkeletonLine
											width={TITLE_WIDTH}
											height={TITLE_HEIGHT}
											emphasis={SkeletonEmphasis.STRONG}
											data-flx="app.skeleton.friends-skeleton.skeleton-line"
										/>
									</flx-app-friends-skeleton-title>
									<flx-app-friends-skeleton-divider
										className={flxElementClassName(styles.divider)}
										data-flx="app.skeleton.friends-skeleton.divider"
									/>
								</>
							)}
							<flx-app-friends-skeleton-tabs
								className={flxElementClassName(styles.tabsWrapper)}
								data-flx="app.skeleton.friends-skeleton.tabs-wrapper"
							>
								<flx-app-friends-skeleton-tab-list
									className={flxElementClassName(styles.tabsInner)}
									data-flx="app.skeleton.friends-skeleton.tabs-inner"
								>
									{tabWidths.map((tabWidth, tabIndex) => (
										<SkeletonBlock
											key={tabIndex}
											width={tabWidth}
											height={TAB_HEIGHT}
											radius={SkeletonRadius.MEDIUM}
											data-flx="app.skeleton.friends-skeleton.skeleton-block--2"
										/>
									))}
								</flx-app-friends-skeleton-tab-list>
							</flx-app-friends-skeleton-tabs>
						</flx-app-friends-skeleton-header-left>
					</flx-app-friends-skeleton-header-left-section>
					<flx-app-friends-skeleton-header-right
						className={flxElementClassName(styles.headerRight)}
						data-flx="app.skeleton.friends-skeleton.header-right"
					>
						{Array.from({length: headerActionCount}, (_unused, actionIndex) => (
							<SkeletonCircle
								key={actionIndex}
								size={HEADER_ACTION_SIZE}
								emphasis={SkeletonEmphasis.MUTED}
								data-flx="app.skeleton.friends-skeleton.skeleton-circle"
							/>
						))}
					</flx-app-friends-skeleton-header-right>
				</flx-app-friends-skeleton-header>
				<flx-app-friends-skeleton-content
					className={flxElementClassName(styles.content)}
					data-flx="app.skeleton.friends-skeleton.content"
				>
					{showSearchBar && (
						<flx-app-friends-skeleton-search
							className={flxElementClassName(styles.searchWrapper)}
							data-flx="app.skeleton.friends-skeleton.search-wrapper"
						>
							<SkeletonBlock
								height={SEARCH_HEIGHT}
								radius={SkeletonRadius.LARGE}
								emphasis={SkeletonEmphasis.MUTED}
								data-flx="app.skeleton.friends-skeleton.skeleton-block--3"
							/>
						</flx-app-friends-skeleton-search>
					)}
					<flx-app-friends-skeleton-tab-body
						className={flxElementClassName(styles.tabBody)}
						data-flx="app.skeleton.friends-skeleton.tab-body"
					>
						{rowCount === 0 ? (
							<FriendsListEmptyStateSkeleton data-flx="app.skeleton.friends-skeleton.friends-list-empty-state-skeleton" />
						) : (
							<flx-app-friends-skeleton-list
								className={flxElementClassName(styles.list)}
								data-flx="app.skeleton.friends-skeleton.list"
							>
								{listSections.map((section, sectionIndex) => (
									<FriendsListSectionSkeleton
										key={sectionIndex}
										rows={section.rows}
										actionCount={section.actionCount}
										withMargin={section.withMargin}
										data-flx="app.skeleton.friends-skeleton.friends-list-section-skeleton"
									/>
								))}
							</flx-app-friends-skeleton-list>
						)}
					</flx-app-friends-skeleton-tab-body>
				</flx-app-friends-skeleton-content>
			</flx-app-friends-skeleton-main>
			{showActiveNow && (
				<flx-app-friends-skeleton-sidebar
					className={flxElementClassName(styles.sidebar)}
					data-flx="app.skeleton.friends-skeleton.sidebar"
				>
					<flx-app-friends-skeleton-sidebar-header
						className={flxElementClassName(styles.sidebarHeader)}
						data-flx="app.skeleton.friends-skeleton.sidebar-header"
					>
						<SkeletonLine
							width={SIDEBAR_TITLE_WIDTH}
							height={SIDEBAR_TITLE_HEIGHT}
							emphasis={SkeletonEmphasis.STRONG}
							data-flx="app.skeleton.friends-skeleton.skeleton-line--2"
						/>
					</flx-app-friends-skeleton-sidebar-header>
					{activeNowCardSpecs.length === 0 ? (
						<ActiveNowEmptyStateSkeleton data-flx="app.skeleton.friends-skeleton.active-now-empty-state-skeleton" />
					) : (
						<flx-app-friends-skeleton-sidebar-content
							className={flxElementClassName(styles.sidebarContent)}
							data-flx="app.skeleton.friends-skeleton.sidebar-content"
						>
							{activeNowCardSpecs.map((spec, index) => (
								<ActiveNowCardSkeleton
									key={index}
									spec={spec}
									data-flx="app.skeleton.friends-skeleton.active-now-card-skeleton"
								/>
							))}
						</flx-app-friends-skeleton-sidebar-content>
					)}
				</flx-app-friends-skeleton-sidebar>
			)}
		</flx-app-friends-skeleton>
	);
});
