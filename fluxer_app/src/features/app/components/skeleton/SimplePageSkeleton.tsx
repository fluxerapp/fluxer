// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/skeleton/SimplePageSkeleton.module.css';
import {SkeletonBlock} from '@app/features/app/components/skeleton/SkeletonBlock';
import {SkeletonCircle} from '@app/features/app/components/skeleton/SkeletonCircle';
import {
	getRememberedSkeletonSimplePageLayout,
	type RememberedSkeletonSimplePageLayout,
	resolveDefaultSkeletonSimplePageLayout,
	SkeletonSimplePageBody,
	SkeletonSimplePageRoute,
} from '@app/features/app/components/skeleton/SkeletonLayoutMemory';
import {SkeletonLine} from '@app/features/app/components/skeleton/SkeletonLine';
import {createSkeletonRandomFromKey} from '@app/features/app/components/skeleton/SkeletonSeed';
import {SkeletonEmphasis, SkeletonRadius} from '@app/features/app/components/skeleton/SkeletonStyle';
import {DEFAULT_PAGE_SIZE} from '@app/features/channel/components/guild_members_page/GuildMembersPageShared';
import {flxElementClassName} from '@app/lib/react';
import {Fragment, useMemo, useState} from 'react';

const MESSAGE_ROW_HEIGHT_PX = 60;
const MEMBER_TABLE_ROW_HEIGHT_PX = 49;
const CHANNEL_CARD_HEIGHT_PX = 256;
const MAX_DERIVED_ROW_COUNT = 64;
const HEADER_ICON_SIZE = '1.5rem';
const HEADER_TITLE_WIDTH = '7rem';
const HEADER_TITLE_HEIGHT = '0.875rem';
const HEADER_FILTER_WIDTH = '6.5rem';
const HEADER_ACTION_SIZE = '2rem';
const ROW_AVATAR_SIZE = '2.25rem';
const ROW_NAME_HEIGHT = '0.75rem';
const ROW_SUBTEXT_HEIGHT = '0.625rem';
const ROW_META_HEIGHT = '0.625rem';
const ROW_NAME_WIDTH_MIN = 30;
const ROW_NAME_WIDTH_RANGE = 36;
const ROW_SUBTEXT_WIDTH_MIN = 45;
const ROW_SUBTEXT_WIDTH_RANGE = 45;
const ROW_META_WIDTH_MIN = 2.5;
const ROW_META_WIDTH_RANGE = 1.5;
const TABLE_HEAD_LABEL_WIDTHS: ReadonlyArray<string> = ['4rem', '5.5rem', '6rem', '5.5rem', '3rem', '3.5rem'];
const TABLE_SELECT_COLUMN_LABEL_WIDTH = '1rem';
const TABLE_SELECT_BOX_SIZE = '1.125rem';
const TABLE_FOOTER_PAGE_SIZE_WIDTH = '5.5rem';
const TABLE_FOOTER_PAGE_SIZE_HEIGHT = '2.75rem';
const TABLE_FOOTER_PAGE_BUTTON_SIZE = '2rem';
const TABLE_FOOTER_PAGE_BUTTON_COUNT = 5;
const TABLE_FOOTER_LABEL_WIDTH = '5.75rem';
const TABLE_FOOTER_LABEL_HEIGHT = '0.6875rem';
const TABLE_HEAD_LABEL_HEIGHT = '0.625rem';
const TABLE_AVATAR_SIZE = '2rem';
const TABLE_NAME_HEIGHT = '0.75rem';
const TABLE_TAG_HEIGHT = '0.625rem';
const TABLE_CELL_HEIGHT = '0.6875rem';
const TABLE_ACTIONS_SIZE = '1.5rem';
const TOOLBAR_TITLE_WIDTH = '5rem';
const TOOLBAR_TITLE_HEIGHT = '0.875rem';
const TOOLBAR_SUBTITLE_WIDTH = '7rem';
const TOOLBAR_SUBTITLE_HEIGHT = '0.6875rem';
const TOOLBAR_CONTROL_HEIGHT = '2.75rem';
const TOOLBAR_SEARCH_WIDTH = '14rem';
const TOOLBAR_SORT_WIDTH = '5.5rem';
const CHANNEL_GROUP_HEADING_WIDTH = '6rem';
const CHANNEL_GROUP_HEADING_HEIGHT = '0.625rem';
const CHANNEL_CARD_ICON_SIZE = '1.25rem';
const CHANNEL_CARD_TITLE_WIDTH = '9rem';
const CHANNEL_CARD_TITLE_HEIGHT = '0.875rem';
const CHANNEL_CARD_MESSAGE_COUNT = 2;
const CHANNEL_CARD_FOOTER_WIDTH = '5.5rem';
const CHANNEL_CARD_FOOTER_HEIGHT = '1.75rem';
const CHANNELS_PER_GROUP = 2;

export interface SimplePageSkeletonProps {
	readonly route: SkeletonSimplePageRoute;
}

interface SimplePageRowSpec {
	readonly nameWidth: string;
	readonly subtextWidth: string;
	readonly metaWidth: string;
}

const BODY_ROW_HEIGHTS_PX: Readonly<Record<SkeletonSimplePageBody, number>> = Object.freeze({
	[SkeletonSimplePageBody.MEMBER_TABLE]: MEMBER_TABLE_ROW_HEIGHT_PX,
	[SkeletonSimplePageBody.MESSAGE_LIST]: MESSAGE_ROW_HEIGHT_PX,
	[SkeletonSimplePageBody.CHANNEL_LIST]: CHANNEL_CARD_HEIGHT_PX,
});

function resolveViewportRowCount(body: SkeletonSimplePageBody): number {
	if (typeof window === 'undefined') {
		return 0;
	}
	const rowHeightPx = BODY_ROW_HEIGHTS_PX[body];
	return Math.min(MAX_DERIVED_ROW_COUNT, Math.max(1, Math.ceil(window.innerHeight / rowHeightPx)));
}

function resolveSimplePageLayout(route: SkeletonSimplePageRoute): RememberedSkeletonSimplePageLayout {
	const rememberedLayout = getRememberedSkeletonSimplePageLayout(route);
	if (rememberedLayout != null) {
		return rememberedLayout;
	}
	const defaultLayout = resolveDefaultSkeletonSimplePageLayout(route);
	return {body: defaultLayout.body, rowCount: resolveViewportRowCount(defaultLayout.body), selectable: false};
}

function resolveRowSpecCount(layout: RememberedSkeletonSimplePageLayout): number {
	if (layout.body === SkeletonSimplePageBody.CHANNEL_LIST) {
		return layout.rowCount * CHANNEL_CARD_MESSAGE_COUNT;
	}
	return layout.rowCount;
}

function createRowSpecs(rowCount: number): ReadonlyArray<SimplePageRowSpec> {
	const random = createSkeletonRandomFromKey('simple-page-skeleton-rows');
	return Array.from({length: rowCount}, () => ({
		nameWidth: `${ROW_NAME_WIDTH_MIN + random() * ROW_NAME_WIDTH_RANGE}%`,
		subtextWidth: `${ROW_SUBTEXT_WIDTH_MIN + random() * ROW_SUBTEXT_WIDTH_RANGE}%`,
		metaWidth: `${ROW_META_WIDTH_MIN + random() * ROW_META_WIDTH_RANGE}rem`,
	}));
}

function SimplePageSkeletonRow({row}: {readonly row: SimplePageRowSpec}) {
	return (
		<flx-app-simple-page-skeleton-row
			className={flxElementClassName(styles.row)}
			data-flx="app.skeleton.simple-page-skeleton.simple-page-skeleton-row.row"
		>
			<SkeletonCircle
				size={ROW_AVATAR_SIZE}
				data-flx="app.skeleton.simple-page-skeleton.simple-page-skeleton-row.skeleton-circle"
			/>
			<flx-app-simple-page-skeleton-row-details
				className={flxElementClassName(styles.rowDetails)}
				data-flx="app.skeleton.simple-page-skeleton.simple-page-skeleton-row.row-details"
			>
				<SkeletonLine
					width={row.nameWidth}
					height={ROW_NAME_HEIGHT}
					emphasis={SkeletonEmphasis.STRONG}
					data-flx="app.skeleton.simple-page-skeleton.simple-page-skeleton-row.skeleton-line"
				/>
				<SkeletonLine
					width={row.subtextWidth}
					height={ROW_SUBTEXT_HEIGHT}
					emphasis={SkeletonEmphasis.MUTED}
					data-flx="app.skeleton.simple-page-skeleton.simple-page-skeleton-row.skeleton-line--2"
				/>
			</flx-app-simple-page-skeleton-row-details>
			<SkeletonLine
				width={row.metaWidth}
				height={ROW_META_HEIGHT}
				emphasis={SkeletonEmphasis.MUTED}
				data-flx="app.skeleton.simple-page-skeleton.simple-page-skeleton-row.skeleton-line--3"
			/>
		</flx-app-simple-page-skeleton-row>
	);
}

function SimplePageMessageListSkeleton({rows}: {readonly rows: ReadonlyArray<SimplePageRowSpec>}) {
	return (
		<flx-app-simple-page-skeleton-rows
			className={flxElementClassName(styles.rows)}
			data-flx="app.skeleton.simple-page-skeleton.simple-page-message-list-skeleton.rows"
		>
			{rows.map((row, index) => (
				<SimplePageSkeletonRow
					key={index}
					row={row}
					data-flx="app.skeleton.simple-page-skeleton.simple-page-message-list-skeleton.simple-page-skeleton-row"
				/>
			))}
		</flx-app-simple-page-skeleton-rows>
	);
}

function SimplePageChannelCardSkeleton({rows}: {readonly rows: ReadonlyArray<SimplePageRowSpec>}) {
	return (
		<flx-app-simple-page-skeleton-channel-card
			className={flxElementClassName(styles.channelCard)}
			data-flx="app.skeleton.simple-page-skeleton.simple-page-channel-card-skeleton.channel-card"
		>
			<flx-app-simple-page-skeleton-channel-card-header
				className={flxElementClassName(styles.channelCardHeader)}
				data-flx="app.skeleton.simple-page-skeleton.simple-page-channel-card-skeleton.channel-card-header"
			>
				<SkeletonBlock
					width={CHANNEL_CARD_ICON_SIZE}
					height={CHANNEL_CARD_ICON_SIZE}
					radius={SkeletonRadius.SMALL}
					emphasis={SkeletonEmphasis.MUTED}
					data-flx="app.skeleton.simple-page-skeleton.simple-page-channel-card-skeleton.skeleton-block"
				/>
				<SkeletonLine
					width={CHANNEL_CARD_TITLE_WIDTH}
					height={CHANNEL_CARD_TITLE_HEIGHT}
					emphasis={SkeletonEmphasis.STRONG}
					data-flx="app.skeleton.simple-page-skeleton.simple-page-channel-card-skeleton.skeleton-line"
				/>
			</flx-app-simple-page-skeleton-channel-card-header>
			<flx-app-simple-page-skeleton-channel-card-stream
				className={flxElementClassName(styles.channelCardStream)}
				data-flx="app.skeleton.simple-page-skeleton.simple-page-channel-card-skeleton.channel-card-stream"
			>
				{rows.map((row, index) => (
					<SimplePageSkeletonRow
						key={index}
						row={row}
						data-flx="app.skeleton.simple-page-skeleton.simple-page-channel-card-skeleton.simple-page-skeleton-row"
					/>
				))}
			</flx-app-simple-page-skeleton-channel-card-stream>
			<flx-app-simple-page-skeleton-channel-card-footer
				className={flxElementClassName(styles.channelCardFooter)}
				data-flx="app.skeleton.simple-page-skeleton.simple-page-channel-card-skeleton.channel-card-footer"
			>
				<SkeletonBlock
					width={CHANNEL_CARD_FOOTER_WIDTH}
					height={CHANNEL_CARD_FOOTER_HEIGHT}
					radius={SkeletonRadius.MEDIUM}
					emphasis={SkeletonEmphasis.MUTED}
					data-flx="app.skeleton.simple-page-skeleton.simple-page-channel-card-skeleton.skeleton-block--2"
				/>
			</flx-app-simple-page-skeleton-channel-card-footer>
		</flx-app-simple-page-skeleton-channel-card>
	);
}

function SimplePageChannelListSkeleton({rows}: {readonly rows: ReadonlyArray<SimplePageRowSpec>}) {
	const cards = useMemo(() => {
		const cardCount = Math.ceil(rows.length / CHANNEL_CARD_MESSAGE_COUNT);
		return Array.from({length: cardCount}, (_unused, cardIndex) =>
			rows.slice(cardIndex * CHANNEL_CARD_MESSAGE_COUNT, (cardIndex + 1) * CHANNEL_CARD_MESSAGE_COUNT),
		);
	}, [rows]);
	return (
		<flx-app-simple-page-skeleton-channel-groups
			className={flxElementClassName(styles.channelGroups)}
			data-flx="app.skeleton.simple-page-skeleton.simple-page-channel-list-skeleton.channel-groups"
		>
			{cards.map((cardRows, cardIndex) => (
				<Fragment key={cardIndex}>
					{cardIndex % CHANNELS_PER_GROUP === 0 && (
						<flx-app-simple-page-skeleton-channel-group-heading
							className={flxElementClassName(styles.channelGroupHeading)}
							data-flx="app.skeleton.simple-page-skeleton.simple-page-channel-list-skeleton.channel-group-heading"
						>
							<SkeletonLine
								width={CHANNEL_GROUP_HEADING_WIDTH}
								height={CHANNEL_GROUP_HEADING_HEIGHT}
								emphasis={SkeletonEmphasis.MUTED}
								data-flx="app.skeleton.simple-page-skeleton.simple-page-channel-list-skeleton.skeleton-line"
							/>
						</flx-app-simple-page-skeleton-channel-group-heading>
					)}
					<SimplePageChannelCardSkeleton
						rows={cardRows}
						data-flx="app.skeleton.simple-page-skeleton.simple-page-channel-list-skeleton.simple-page-channel-card-skeleton"
					/>
				</Fragment>
			))}
		</flx-app-simple-page-skeleton-channel-groups>
	);
}

function SimplePageMemberTableRowSkeleton({
	row,
	selectable,
}: {
	readonly row: SimplePageRowSpec;
	readonly selectable: boolean;
}) {
	return (
		<flx-app-simple-page-skeleton-table-row
			className={flxElementClassName(styles.tableRow)}
			data-flx="app.skeleton.simple-page-skeleton.simple-page-member-table-row-skeleton.table-row"
		>
			{selectable && (
				<flx-app-simple-page-skeleton-table-cell
					className={flxElementClassName(styles.tableCell)}
					data-flx="app.skeleton.simple-page-skeleton.simple-page-member-table-row-skeleton.table-cell"
				>
					<SkeletonBlock
						width={TABLE_SELECT_BOX_SIZE}
						height={TABLE_SELECT_BOX_SIZE}
						radius={SkeletonRadius.SMALL}
						emphasis={SkeletonEmphasis.MUTED}
						data-flx="app.skeleton.simple-page-skeleton.simple-page-member-table-row-skeleton.skeleton-block"
					/>
				</flx-app-simple-page-skeleton-table-cell>
			)}
			<flx-app-simple-page-skeleton-table-cell
				className={flxElementClassName(styles.tableCell)}
				data-flx="app.skeleton.simple-page-skeleton.simple-page-member-table-row-skeleton.table-cell--2"
			>
				<flx-app-simple-page-skeleton-table-name
					className={flxElementClassName(styles.tableNameCell)}
					data-flx="app.skeleton.simple-page-skeleton.simple-page-member-table-row-skeleton.table-name-cell"
				>
					<SkeletonCircle
						size={TABLE_AVATAR_SIZE}
						data-flx="app.skeleton.simple-page-skeleton.simple-page-member-table-row-skeleton.skeleton-circle"
					/>
					<flx-app-simple-page-skeleton-table-name-info
						className={flxElementClassName(styles.tableNameInfo)}
						data-flx="app.skeleton.simple-page-skeleton.simple-page-member-table-row-skeleton.table-name-info"
					>
						<SkeletonLine
							width={row.nameWidth}
							height={TABLE_NAME_HEIGHT}
							emphasis={SkeletonEmphasis.STRONG}
							data-flx="app.skeleton.simple-page-skeleton.simple-page-member-table-row-skeleton.skeleton-line"
						/>
						<SkeletonLine
							width={row.metaWidth}
							height={TABLE_TAG_HEIGHT}
							emphasis={SkeletonEmphasis.MUTED}
							data-flx="app.skeleton.simple-page-skeleton.simple-page-member-table-row-skeleton.skeleton-line--2"
						/>
					</flx-app-simple-page-skeleton-table-name-info>
				</flx-app-simple-page-skeleton-table-name>
			</flx-app-simple-page-skeleton-table-cell>
			<flx-app-simple-page-skeleton-table-cell
				className={flxElementClassName(styles.tableCell)}
				data-flx="app.skeleton.simple-page-skeleton.simple-page-member-table-row-skeleton.table-cell--3"
			>
				<SkeletonLine
					width={row.metaWidth}
					height={TABLE_CELL_HEIGHT}
					emphasis={SkeletonEmphasis.MUTED}
					data-flx="app.skeleton.simple-page-skeleton.simple-page-member-table-row-skeleton.skeleton-line--3"
				/>
			</flx-app-simple-page-skeleton-table-cell>
			<flx-app-simple-page-skeleton-table-cell
				className={flxElementClassName(styles.tableCell)}
				data-flx="app.skeleton.simple-page-skeleton.simple-page-member-table-row-skeleton.table-cell--4"
			>
				<SkeletonLine
					width={row.metaWidth}
					height={TABLE_CELL_HEIGHT}
					emphasis={SkeletonEmphasis.MUTED}
					data-flx="app.skeleton.simple-page-skeleton.simple-page-member-table-row-skeleton.skeleton-line--4"
				/>
			</flx-app-simple-page-skeleton-table-cell>
			<flx-app-simple-page-skeleton-table-cell
				className={flxElementClassName(styles.tableCell)}
				data-flx="app.skeleton.simple-page-skeleton.simple-page-member-table-row-skeleton.table-cell--5"
			>
				<SkeletonLine
					width={row.subtextWidth}
					height={TABLE_CELL_HEIGHT}
					emphasis={SkeletonEmphasis.MUTED}
					data-flx="app.skeleton.simple-page-skeleton.simple-page-member-table-row-skeleton.skeleton-line--5"
				/>
			</flx-app-simple-page-skeleton-table-cell>
			<flx-app-simple-page-skeleton-table-cell
				className={flxElementClassName(styles.tableCell)}
				data-flx="app.skeleton.simple-page-skeleton.simple-page-member-table-row-skeleton.table-cell--6"
			>
				<SkeletonLine
					width={row.nameWidth}
					height={TABLE_CELL_HEIGHT}
					emphasis={SkeletonEmphasis.MUTED}
					data-flx="app.skeleton.simple-page-skeleton.simple-page-member-table-row-skeleton.skeleton-line--6"
				/>
			</flx-app-simple-page-skeleton-table-cell>
			<flx-app-simple-page-skeleton-table-cell
				className={flxElementClassName(styles.tableCell, styles.tableActionsCell)}
				data-flx="app.skeleton.simple-page-skeleton.simple-page-member-table-row-skeleton.table-cell--7"
			>
				<SkeletonCircle
					size={TABLE_ACTIONS_SIZE}
					emphasis={SkeletonEmphasis.MUTED}
					data-flx="app.skeleton.simple-page-skeleton.simple-page-member-table-row-skeleton.skeleton-circle--2"
				/>
			</flx-app-simple-page-skeleton-table-cell>
		</flx-app-simple-page-skeleton-table-row>
	);
}

function SimplePageMemberTableFooterSkeleton({paginationVisible}: {readonly paginationVisible: boolean}) {
	return (
		<flx-app-simple-page-skeleton-table-footer
			className={flxElementClassName(styles.tableFooter)}
			data-flx="app.skeleton.simple-page-skeleton.simple-page-member-table-footer-skeleton.table-footer"
		>
			<flx-app-simple-page-skeleton-table-footer-left
				className={flxElementClassName(styles.tableFooterLeft)}
				data-flx="app.skeleton.simple-page-skeleton.simple-page-member-table-footer-skeleton.table-footer-left"
			>
				<SkeletonLine
					width={TABLE_FOOTER_LABEL_WIDTH}
					height={TABLE_FOOTER_LABEL_HEIGHT}
					emphasis={SkeletonEmphasis.MUTED}
					data-flx="app.skeleton.simple-page-skeleton.simple-page-member-table-footer-skeleton.skeleton-line"
				/>
				<SkeletonBlock
					width={TABLE_FOOTER_PAGE_SIZE_WIDTH}
					height={TABLE_FOOTER_PAGE_SIZE_HEIGHT}
					radius={SkeletonRadius.LARGE}
					emphasis={SkeletonEmphasis.MUTED}
					data-flx="app.skeleton.simple-page-skeleton.simple-page-member-table-footer-skeleton.skeleton-block"
				/>
			</flx-app-simple-page-skeleton-table-footer-left>
			{paginationVisible && (
				<flx-app-simple-page-skeleton-table-footer-right
					className={flxElementClassName(styles.tableFooterRight)}
					data-flx="app.skeleton.simple-page-skeleton.simple-page-member-table-footer-skeleton.table-footer-right"
				>
					{Array.from({length: TABLE_FOOTER_PAGE_BUTTON_COUNT}, (_unused, buttonIndex) => (
						<SkeletonCircle
							key={buttonIndex}
							size={TABLE_FOOTER_PAGE_BUTTON_SIZE}
							emphasis={SkeletonEmphasis.MUTED}
							data-flx="app.skeleton.simple-page-skeleton.simple-page-member-table-footer-skeleton.skeleton-circle"
						/>
					))}
				</flx-app-simple-page-skeleton-table-footer-right>
			)}
		</flx-app-simple-page-skeleton-table-footer>
	);
}

function SimplePageMemberTableSkeleton({
	rows,
	selectable,
}: {
	readonly rows: ReadonlyArray<SimplePageRowSpec>;
	readonly selectable: boolean;
}) {
	return (
		<flx-app-simple-page-skeleton-table-content
			className={flxElementClassName(styles.tableContent)}
			data-flx="app.skeleton.simple-page-skeleton.simple-page-member-table-skeleton.table-content"
		>
			<flx-app-simple-page-skeleton-toolbar
				className={flxElementClassName(styles.toolbar)}
				data-flx="app.skeleton.simple-page-skeleton.simple-page-member-table-skeleton.toolbar"
			>
				<flx-app-simple-page-skeleton-toolbar-left
					className={flxElementClassName(styles.toolbarLeft)}
					data-flx="app.skeleton.simple-page-skeleton.simple-page-member-table-skeleton.toolbar-left"
				>
					<SkeletonLine
						width={TOOLBAR_TITLE_WIDTH}
						height={TOOLBAR_TITLE_HEIGHT}
						emphasis={SkeletonEmphasis.STRONG}
						data-flx="app.skeleton.simple-page-skeleton.simple-page-member-table-skeleton.skeleton-line"
					/>
					<SkeletonLine
						width={TOOLBAR_SUBTITLE_WIDTH}
						height={TOOLBAR_SUBTITLE_HEIGHT}
						emphasis={SkeletonEmphasis.MUTED}
						data-flx="app.skeleton.simple-page-skeleton.simple-page-member-table-skeleton.skeleton-line--2"
					/>
				</flx-app-simple-page-skeleton-toolbar-left>
				<flx-app-simple-page-skeleton-toolbar-right
					className={flxElementClassName(styles.toolbarRight)}
					data-flx="app.skeleton.simple-page-skeleton.simple-page-member-table-skeleton.toolbar-right"
				>
					<SkeletonBlock
						width={TOOLBAR_SEARCH_WIDTH}
						height={TOOLBAR_CONTROL_HEIGHT}
						radius={SkeletonRadius.LARGE}
						emphasis={SkeletonEmphasis.MUTED}
						data-flx="app.skeleton.simple-page-skeleton.simple-page-member-table-skeleton.skeleton-block"
					/>
					<SkeletonBlock
						width={TOOLBAR_SORT_WIDTH}
						height={TOOLBAR_CONTROL_HEIGHT}
						radius={SkeletonRadius.LARGE}
						emphasis={SkeletonEmphasis.MUTED}
						data-flx="app.skeleton.simple-page-skeleton.simple-page-member-table-skeleton.skeleton-block--2"
					/>
				</flx-app-simple-page-skeleton-toolbar-right>
			</flx-app-simple-page-skeleton-toolbar>
			<flx-app-simple-page-skeleton-table-wrapper
				className={flxElementClassName(styles.tableWrapper)}
				data-flx="app.skeleton.simple-page-skeleton.simple-page-member-table-skeleton.table-wrapper"
			>
				<flx-app-simple-page-skeleton-table-viewport
					className={flxElementClassName(styles.tableViewport)}
					data-flx="app.skeleton.simple-page-skeleton.simple-page-member-table-skeleton.table-viewport"
				>
					<flx-app-simple-page-skeleton-table-surface
						className={flxElementClassName(styles.tableSurface, selectable && styles.tableSurfaceSelectable)}
						data-flx="app.skeleton.simple-page-skeleton.simple-page-member-table-skeleton.table-surface"
					>
						<flx-app-simple-page-skeleton-table-head
							className={flxElementClassName(styles.tableHeadRow)}
							data-flx="app.skeleton.simple-page-skeleton.simple-page-member-table-skeleton.table-head-row"
						>
							{selectable && (
								<flx-app-simple-page-skeleton-table-head-cell
									className={flxElementClassName(styles.tableHeadCell)}
									data-flx="app.skeleton.simple-page-skeleton.simple-page-member-table-skeleton.table-head-cell"
								>
									<SkeletonLine
										width={TABLE_SELECT_COLUMN_LABEL_WIDTH}
										height={TABLE_HEAD_LABEL_HEIGHT}
										emphasis={SkeletonEmphasis.MUTED}
										data-flx="app.skeleton.simple-page-skeleton.simple-page-member-table-skeleton.skeleton-line--3"
									/>
								</flx-app-simple-page-skeleton-table-head-cell>
							)}
							{TABLE_HEAD_LABEL_WIDTHS.map((labelWidth, columnIndex) => (
								<flx-app-simple-page-skeleton-table-head-cell
									key={columnIndex}
									className={flxElementClassName(styles.tableHeadCell)}
									data-flx="app.skeleton.simple-page-skeleton.simple-page-member-table-skeleton.table-head-cell--2"
								>
									<SkeletonLine
										width={labelWidth}
										height={TABLE_HEAD_LABEL_HEIGHT}
										emphasis={SkeletonEmphasis.MUTED}
										data-flx="app.skeleton.simple-page-skeleton.simple-page-member-table-skeleton.skeleton-line--4"
									/>
								</flx-app-simple-page-skeleton-table-head-cell>
							))}
						</flx-app-simple-page-skeleton-table-head>
						<flx-app-simple-page-skeleton-table-body
							className={flxElementClassName(styles.tableBody)}
							data-flx="app.skeleton.simple-page-skeleton.simple-page-member-table-skeleton.table-body"
						>
							{rows.map((row, index) => (
								<SimplePageMemberTableRowSkeleton
									key={index}
									row={row}
									selectable={selectable}
									data-flx="app.skeleton.simple-page-skeleton.simple-page-member-table-skeleton.simple-page-member-table-row-skeleton"
								/>
							))}
						</flx-app-simple-page-skeleton-table-body>
					</flx-app-simple-page-skeleton-table-surface>
				</flx-app-simple-page-skeleton-table-viewport>
				<SimplePageMemberTableFooterSkeleton
					paginationVisible={rows.length >= DEFAULT_PAGE_SIZE}
					data-flx="app.skeleton.simple-page-skeleton.simple-page-member-table-skeleton.simple-page-member-table-footer-skeleton"
				/>
			</flx-app-simple-page-skeleton-table-wrapper>
		</flx-app-simple-page-skeleton-table-content>
	);
}

function renderSimplePageBody(
	body: SkeletonSimplePageBody,
	rows: ReadonlyArray<SimplePageRowSpec>,
	selectable: boolean,
) {
	switch (body) {
		case SkeletonSimplePageBody.MEMBER_TABLE:
			return (
				<SimplePageMemberTableSkeleton
					rows={rows}
					selectable={selectable}
					data-flx="app.skeleton.simple-page-skeleton.render-simple-page-body.simple-page-member-table-skeleton"
				/>
			);
		case SkeletonSimplePageBody.CHANNEL_LIST:
			return (
				<SimplePageChannelListSkeleton
					rows={rows}
					data-flx="app.skeleton.simple-page-skeleton.render-simple-page-body.simple-page-channel-list-skeleton"
				/>
			);
		case SkeletonSimplePageBody.MESSAGE_LIST:
			return (
				<SimplePageMessageListSkeleton
					rows={rows}
					data-flx="app.skeleton.simple-page-skeleton.render-simple-page-body.simple-page-message-list-skeleton"
				/>
			);
	}
}

export const SimplePageSkeleton = ({route}: SimplePageSkeletonProps) => {
	const [layout] = useState(() => resolveSimplePageLayout(route));
	const compactHeader = route === SkeletonSimplePageRoute.NOTIFICATIONS;
	const rows = useMemo(() => createRowSpecs(resolveRowSpecCount(layout)), [layout]);
	return (
		<flx-app-simple-page-skeleton
			className={flxElementClassName(styles.container)}
			aria-hidden
			data-flx="app.skeleton.simple-page-skeleton.container"
		>
			<flx-app-simple-page-skeleton-header
				className={flxElementClassName(styles.header, compactHeader && styles.headerCompact)}
				data-flx="app.skeleton.simple-page-skeleton.header"
			>
				<flx-app-simple-page-skeleton-header-leading
					className={flxElementClassName(styles.headerLeading)}
					data-flx="app.skeleton.simple-page-skeleton.header-leading"
				>
					{!compactHeader && (
						<SkeletonBlock
							width={HEADER_ICON_SIZE}
							height={HEADER_ICON_SIZE}
							radius={SkeletonRadius.SMALL}
							emphasis={SkeletonEmphasis.STRONG}
							data-flx="app.skeleton.simple-page-skeleton.skeleton-block"
						/>
					)}
					<SkeletonLine
						width={HEADER_TITLE_WIDTH}
						height={HEADER_TITLE_HEIGHT}
						emphasis={SkeletonEmphasis.STRONG}
						data-flx="app.skeleton.simple-page-skeleton.skeleton-line"
					/>
				</flx-app-simple-page-skeleton-header-leading>
				{compactHeader && (
					<flx-app-simple-page-skeleton-header-actions
						className={flxElementClassName(styles.headerActions)}
						data-flx="app.skeleton.simple-page-skeleton.header-actions"
					>
						<SkeletonBlock
							width={HEADER_FILTER_WIDTH}
							height={HEADER_ACTION_SIZE}
							radius={SkeletonRadius.LARGE}
							emphasis={SkeletonEmphasis.MUTED}
							data-flx="app.skeleton.simple-page-skeleton.skeleton-block--2"
						/>
						<SkeletonCircle
							size={HEADER_ACTION_SIZE}
							emphasis={SkeletonEmphasis.MUTED}
							data-flx="app.skeleton.simple-page-skeleton.skeleton-circle"
						/>
					</flx-app-simple-page-skeleton-header-actions>
				)}
			</flx-app-simple-page-skeleton-header>
			{renderSimplePageBody(layout.body, rows, layout.selectable)}
		</flx-app-simple-page-skeleton>
	);
};
