// SPDX-License-Identifier: AGPL-3.0-or-later

import {SkeletonCircle} from '@app/features/app/components/skeleton/SkeletonCircle';
import {
	type RememberedSkeletonMemberGroup,
	SKELETON_UNMEASURED_WIDTH_PX,
} from '@app/features/app/components/skeleton/SkeletonLayoutMemory';
import {SkeletonLine} from '@app/features/app/components/skeleton/SkeletonLine';
import {createSkeletonRandomFromKey} from '@app/features/app/components/skeleton/SkeletonSeed';
import {SkeletonEmphasis} from '@app/features/app/components/skeleton/SkeletonStyle';
import {MEMBER_LIST_METRICS_STYLE} from '@app/features/channel/components/MemberListMetrics';
import styles from '@app/features/channel/components/MemberListSkeleton.module.css';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {flxElementClassName} from '@app/lib/react';

const AVATAR_SIZE = '2rem';
const GROUP_HEADING_HEIGHT = '0.625rem';
const NAME_HEIGHT = '0.625rem';
const STATUS_HEIGHT = '0.5rem';
const NAME_WIDTH_MIN = 40;
const NAME_WIDTH_RANGE = 40;
const STATUS_WIDTH_MIN = 30;
const STATUS_WIDTH_RANGE = 50;

export const MemberListSkeletonVariant = Object.freeze({
	GUILD: 'guild',
	GROUP_DM: 'group-dm',
} as const);

export type MemberListSkeletonVariant = (typeof MemberListSkeletonVariant)[keyof typeof MemberListSkeletonVariant];

interface MemberListSkeletonRowSpec {
	readonly nameWidth: number;
	readonly statusWidth: number;
}

interface MemberListSkeletonGroupRequest {
	readonly headingWidth: string;
	readonly rowCount: number;
	readonly subtextFlags: ReadonlyArray<boolean>;
}

const NO_SUBTEXT_FLAGS: ReadonlyArray<boolean> = Object.freeze([]);

const GUILD_GROUP_REQUESTS: ReadonlyArray<MemberListSkeletonGroupRequest> = [
	{headingWidth: '5rem', rowCount: 7, subtextFlags: NO_SUBTEXT_FLAGS},
	{headingWidth: '6.25rem', rowCount: 9, subtextFlags: NO_SUBTEXT_FLAGS},
	{headingWidth: '4.5rem', rowCount: 8, subtextFlags: NO_SUBTEXT_FLAGS},
];

const GROUP_DM_GROUP_REQUESTS: ReadonlyArray<MemberListSkeletonGroupRequest> = [
	{headingWidth: '4.5rem', rowCount: 5, subtextFlags: NO_SUBTEXT_FLAGS},
	{headingWidth: '4rem', rowCount: 5, subtextFlags: NO_SUBTEXT_FLAGS},
];

function createMemberListSkeletonRowSpec(
	variant: MemberListSkeletonVariant,
	rowIndex: number,
): MemberListSkeletonRowSpec {
	const random = createSkeletonRandomFromKey(`member-list-skeleton|${variant}|${rowIndex}`);
	return {
		nameWidth: NAME_WIDTH_MIN + random() * NAME_WIDTH_RANGE,
		statusWidth: STATUS_WIDTH_MIN + random() * STATUS_WIDTH_RANGE,
	};
}

function resolveDefaultGroupRequests(
	variant: MemberListSkeletonVariant,
): ReadonlyArray<MemberListSkeletonGroupRequest> {
	if (variant === MemberListSkeletonVariant.GROUP_DM) {
		return GROUP_DM_GROUP_REQUESTS;
	}
	return GUILD_GROUP_REQUESTS;
}

function resolveGroupRequests(
	variant: MemberListSkeletonVariant,
	memberGroups: ReadonlyArray<RememberedSkeletonMemberGroup> | null,
): ReadonlyArray<MemberListSkeletonGroupRequest> {
	const defaults = resolveDefaultGroupRequests(variant);
	if (memberGroups == null) {
		return defaults;
	}
	return memberGroups.map((group, index) => {
		if (group.headingWidthPx === SKELETON_UNMEASURED_WIDTH_PX) {
			return {
				headingWidth: defaults[index % defaults.length].headingWidth,
				rowCount: group.rowCount,
				subtextFlags: group.subtextFlags,
			};
		}
		return {
			headingWidth: remFromPx(group.headingWidthPx),
			rowCount: group.rowCount,
			subtextFlags: group.subtextFlags,
		};
	});
}

interface MemberListSkeletonRowShapeProps {
	readonly row: MemberListSkeletonRowSpec;
	readonly variant: MemberListSkeletonVariant;
	readonly showSubtext: boolean;
}

function MemberListSkeletonRowShape({row, variant, showSubtext}: MemberListSkeletonRowShapeProps) {
	return (
		<flx-channel-member-skeleton
			className={flxElementClassName(styles.row, variant === MemberListSkeletonVariant.GROUP_DM && styles.rowGroupDM)}
			style={MEMBER_LIST_METRICS_STYLE}
			data-flx="channel.member-list-skeleton.member-list-skeleton-row-shape.row"
		>
			<flx-channel-member-skeleton-content
				className={flxElementClassName(styles.rowContent)}
				data-flx="channel.member-list-skeleton.member-list-skeleton-row-shape.row-content"
			>
				<SkeletonCircle
					size={AVATAR_SIZE}
					data-flx="channel.member-list-skeleton.member-list-skeleton-row-shape.skeleton-circle"
				/>
				<flx-channel-member-skeleton-info
					className={flxElementClassName(styles.rowInfo)}
					data-flx="channel.member-list-skeleton.member-list-skeleton-row-shape.row-info"
				>
					<flx-channel-member-skeleton-name
						className={flxElementClassName(styles.nameSlot)}
						data-flx="channel.member-list-skeleton.member-list-skeleton-row-shape.name-slot"
					>
						<SkeletonLine
							width={`${row.nameWidth}%`}
							height={NAME_HEIGHT}
							data-flx="channel.member-list-skeleton.member-list-skeleton-row-shape.skeleton-line"
						/>
					</flx-channel-member-skeleton-name>
					{showSubtext && (
						<flx-channel-member-skeleton-status
							className={flxElementClassName(styles.statusSlot)}
							data-flx="channel.member-list-skeleton.member-list-skeleton-row-shape.status-slot"
						>
							<SkeletonLine
								width={`${row.statusWidth}%`}
								height={STATUS_HEIGHT}
								emphasis={SkeletonEmphasis.MUTED}
								data-flx="channel.member-list-skeleton.member-list-skeleton-row-shape.skeleton-line--2"
							/>
						</flx-channel-member-skeleton-status>
					)}
				</flx-channel-member-skeleton-info>
			</flx-channel-member-skeleton-content>
		</flx-channel-member-skeleton>
	);
}

interface MemberListSkeletonRowProps {
	readonly index: number;
	readonly variant: MemberListSkeletonVariant;
}

export function MemberListSkeletonRow({index, variant}: MemberListSkeletonRowProps) {
	return (
		<MemberListSkeletonRowShape
			row={createMemberListSkeletonRowSpec(variant, index)}
			variant={variant}
			showSubtext={variant !== MemberListSkeletonVariant.GROUP_DM}
			data-flx="channel.member-list-skeleton.member-list-skeleton-row.member-list-skeleton-row-shape"
		/>
	);
}

interface MemberListSkeletonProps {
	readonly variant: MemberListSkeletonVariant;
	readonly memberGroups?: ReadonlyArray<RememberedSkeletonMemberGroup> | null;
}

export function MemberListSkeleton({variant, memberGroups = null}: MemberListSkeletonProps) {
	const groups = resolveGroupRequests(variant, memberGroups);
	let rowIndex = 0;
	return (
		<flx-channel-member-list-skeleton
			className={flxElementClassName(styles.groups)}
			style={MEMBER_LIST_METRICS_STYLE}
			aria-hidden
			data-flx="channel.member-list-skeleton.groups"
		>
			{groups.map((group, groupIndex) => (
				<flx-channel-member-skeleton-group
					key={`${groupIndex}-${group.headingWidth}`}
					className={flxElementClassName(
						styles.group,
						variant === MemberListSkeletonVariant.GROUP_DM && styles.groupGroupDM,
					)}
					data-flx="channel.member-list-skeleton.group"
				>
					<flx-channel-member-skeleton-group-header
						className={flxElementClassName(styles.groupHeader)}
						data-flx="channel.member-list-skeleton.group-header"
					>
						<SkeletonLine
							width={group.headingWidth}
							height={GROUP_HEADING_HEIGHT}
							emphasis={SkeletonEmphasis.MUTED}
							data-flx="channel.member-list-skeleton.skeleton-line"
						/>
					</flx-channel-member-skeleton-group-header>
					<flx-channel-member-skeleton-group-rows
						className={flxElementClassName(
							styles.rows,
							variant === MemberListSkeletonVariant.GROUP_DM && styles.rowsGroupDM,
						)}
						data-flx="channel.member-list-skeleton.rows"
					>
						{Array.from({length: group.rowCount}, (_, rowInGroup) => {
							const index = rowIndex;
							rowIndex += 1;
							return (
								<MemberListSkeletonRowShape
									key={index}
									row={createMemberListSkeletonRowSpec(variant, index)}
									variant={variant}
									showSubtext={
										variant !== MemberListSkeletonVariant.GROUP_DM || group.subtextFlags[rowInGroup] === true
									}
									data-flx="channel.member-list-skeleton.member-list-skeleton-row-shape"
								/>
							);
						})}
					</flx-channel-member-skeleton-group-rows>
				</flx-channel-member-skeleton-group>
			))}
		</flx-channel-member-list-skeleton>
	);
}
