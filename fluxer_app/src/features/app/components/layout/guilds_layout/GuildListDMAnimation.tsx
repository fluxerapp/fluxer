// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/layout/GuildsLayout.module.css';
import {DMListItem} from '@app/features/app/components/layout/sidebar_nav/GuildListDMItem';
import type {Channel} from '@app/features/channel/models/Channel';
import {motion} from 'framer-motion';
import type React from 'react';
import {useEffect, useRef, useState} from 'react';

const DM_LIST_ENTER_X = -88;
export const DM_LIST_ENTER_DURATION_MS = 260;
const DM_LIST_EXIT_DURATION_MS = 200;
export const DM_LIST_REMOVAL_DELAY_MS = DM_LIST_EXIT_DURATION_MS + 120;
const DM_LIST_ENTER_EASING: [number, number, number, number] = [0.165, 0.84, 0.44, 1];
const DM_LIST_EXIT_EASING: [number, number, number, number] = [0.645, 0.045, 0.355, 1];

export interface DMListRow {
	readonly type: 'channel';
	readonly channel: Channel;
	readonly voiceCallActive: boolean;
	readonly pendingRemoval: boolean;
}

interface DMListAnimatedRowProps {
	readonly channel: Channel;
	readonly isLast: boolean;
	readonly isSelected: boolean;
	readonly pendingRemoval: boolean;
	readonly reducedMotion: boolean;
	readonly onHoverStart: (channelId: string) => void;
	readonly onHoverEnd: (channelId: string) => void;
	readonly onRemovalAnimationComplete: (channelId: string) => void;
	readonly scrollAnchorRef: React.RefCallback<HTMLElement>;
	readonly voiceCallActive: boolean;
}

export const DMListAnimatedRow: React.FC<DMListAnimatedRowProps> = ({
	channel,
	isLast,
	isSelected,
	pendingRemoval,
	reducedMotion,
	onHoverStart,
	onHoverEnd,
	onRemovalAnimationComplete,
	scrollAnchorRef,
	voiceCallActive,
}) => {
	const previousPendingRemovalRef = useRef(pendingRemoval);
	const removalCycleRef = useRef(0);
	if (previousPendingRemovalRef.current !== pendingRemoval) {
		previousPendingRemovalRef.current = pendingRemoval;
		removalCycleRef.current += 1;
	}
	const removalCycle = removalCycleRef.current;
	let rowHeight = 'calc(var(--guild-list-item-box-size) + var(--guild-list-item-gap))';
	if (isLast) rowHeight = 'var(--guild-list-item-box-size)';
	let durationMs = DM_LIST_ENTER_DURATION_MS;
	let easing = DM_LIST_ENTER_EASING;
	if (pendingRemoval) {
		durationMs = DM_LIST_EXIT_DURATION_MS;
		easing = DM_LIST_EXIT_EASING;
	}
	if (reducedMotion) durationMs = 0;
	return (
		<motion.div
			ref={scrollAnchorRef}
			className={styles.dmListRow}
			initial={reducedMotion ? false : {height: 0}}
			animate={{height: pendingRemoval ? 0 : rowHeight}}
			transition={{duration: durationMs / 1000, ease: easing}}
			onAnimationComplete={() => {
				if (pendingRemoval && removalCycleRef.current === removalCycle) {
					onRemovalAnimationComplete(channel.id);
				}
			}}
			data-scroll-anchor-key={`dm-${channel.id}`}
			data-pending-removal={pendingRemoval ? 'true' : undefined}
			data-last-row={isLast ? 'true' : undefined}
			data-flx="app.guild-list-dm-animation.dm-list-row"
		>
			<motion.div
				className={styles.dmListItemBox}
				initial={reducedMotion ? false : {x: DM_LIST_ENTER_X}}
				animate={{x: pendingRemoval ? DM_LIST_ENTER_X : 0}}
				transition={{duration: durationMs / 1000, ease: easing}}
				data-flx="app.guild-list-dm-animation.dm-list-item-box"
			>
				<DMListItem
					channel={channel}
					isSelected={isSelected}
					onHoverStart={onHoverStart}
					onHoverEnd={onHoverEnd}
					voiceCallActive={voiceCallActive}
					data-flx="app.guild-list-dm-animation.dm-list-item"
				/>
			</motion.div>
		</motion.div>
	);
};

export function useFrameBatchedDMListRows(rows: ReadonlyArray<DMListRow>): ReadonlyArray<DMListRow> {
	const [batchedRows, setBatchedRows] = useState<ReadonlyArray<DMListRow>>(rows);
	const latestRowsRef = useRef(rows);
	const pendingRowsRef = useRef(rows);
	const batchAnimationFrameRef = useRef<number | null>(null);
	latestRowsRef.current = rows;
	useEffect(() => {
		pendingRowsRef.current = latestRowsRef.current;
		if (globalThis.requestAnimationFrame == null) {
			setBatchedRows(latestRowsRef.current);
			return;
		}
		if (batchAnimationFrameRef.current != null) return;
		batchAnimationFrameRef.current = requestAnimationFrame(() => {
			batchAnimationFrameRef.current = null;
			setBatchedRows(pendingRowsRef.current);
		});
	}, [rows]);
	useEffect(() => {
		return () => {
			if (batchAnimationFrameRef.current != null) {
				cancelAnimationFrame(batchAnimationFrameRef.current);
				batchAnimationFrameRef.current = null;
			}
		};
	}, []);
	return batchedRows;
}
