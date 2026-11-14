// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/layout/GuildsLayout.module.css';
import {
	animateGuildRailDM,
	guildRailDMClosedTransform,
} from '@app/features/app/components/layout/guilds_layout/GuildRailLayoutAnimation';
import {DMListItem} from '@app/features/app/components/layout/sidebar_nav/GuildListDMItem';
import Initialization from '@app/features/app/state/Initialization';
import type {Channel} from '@app/features/channel/models/Channel';
import {useDirection} from '@base-ui/react/direction-provider';
import type React from 'react';
import {useEffect, useLayoutEffect, useRef, useState} from 'react';

const DM_LIST_EXIT_DURATION_MS = 200;
export const DM_LIST_REMOVAL_DELAY_MS = DM_LIST_EXIT_DURATION_MS + 120;

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
	readonly paused: boolean;
	readonly onHoverStart: (channelId: string) => void;
	readonly onHoverEnd: (channelId: string) => void;
	readonly onRemovalAnimationComplete: (channelId: string) => void;
	readonly scrollTargetRef: React.RefCallback<HTMLElement>;
	readonly voiceCallActive: boolean;
}

export const DMListAnimatedRow: React.FC<DMListAnimatedRowProps> = ({
	channel,
	isLast,
	isSelected,
	pendingRemoval,
	reducedMotion,
	paused,
	onHoverStart,
	onHoverEnd,
	onRemovalAnimationComplete,
	scrollTargetRef,
	voiceCallActive,
}) => {
	const setRowRef = scrollTargetRef;
	const rtl = useDirection() === 'rtl';
	const [mountedInRestoredView] = useState(() => Initialization.isStale);
	const skipEnter = reducedMotion || mountedInRestoredView;
	const previousPendingRemovalRef = useRef(pendingRemoval);
	const removalCycleRef = useRef(0);
	if (previousPendingRemovalRef.current !== pendingRemoval) {
		previousPendingRemovalRef.current = pendingRemoval;
		removalCycleRef.current += 1;
	}
	const removalCycle = removalCycleRef.current;
	let rowHeight = 'calc(var(--guild-list-item-box-size) + var(--guild-list-item-gap))';
	if (isLast) rowHeight = 'var(--guild-list-item-box-size)';
	const itemRef = useRef<HTMLDivElement | null>(null);
	const animationRef = useRef<Animation | null>(null);
	const previousFrameRef = useRef<Keyframe | null>(null);
	const initializedRef = useRef(false);
	const pausedRef = useRef(paused);
	pausedRef.current = paused;
	useLayoutEffect(() => {
		const node = itemRef.current;
		if (node == null) return;
		const first = !initializedRef.current;
		initializedRef.current = true;
		const from = previousFrameRef.current;
		previousFrameRef.current = null;
		if (reducedMotion || (!pendingRemoval && !(first && !skipEnter) && from == null)) {
			if (pendingRemoval) onRemovalAnimationComplete(channel.id);
			return;
		}
		const animation = animateGuildRailDM(node, pendingRemoval, from, rtl);
		animationRef.current = animation;
		if (animation == null) {
			if (pendingRemoval) onRemovalAnimationComplete(channel.id);
			return;
		}
		if (pausedRef.current) animation.pause();
		void animation.finished
			.then(() => {
				if (animationRef.current !== animation) return;
				animationRef.current = null;
				animation.cancel();
				if (pendingRemoval && removalCycleRef.current === removalCycle) onRemovalAnimationComplete(channel.id);
			})
			.catch(() => {});
		return () => {
			if (animationRef.current !== animation) return;
			const style = node.ownerDocument.defaultView?.getComputedStyle(node);
			if (style != null) previousFrameRef.current = {transform: style.transform};
			animationRef.current = null;
			animation.cancel();
		};
	}, [channel.id, onRemovalAnimationComplete, pendingRemoval, reducedMotion, removalCycle, rtl, skipEnter]);
	useLayoutEffect(() => {
		const animation = animationRef.current;
		if (paused && animation?.playState === 'running') animation.pause();
		else if (!paused && animation?.playState === 'paused') animation.play();
	}, [paused]);
	return (
		<div
			ref={setRowRef}
			className={styles.dmListRow}
			style={{height: rowHeight, marginBottom: pendingRemoval ? `calc(-1 * ${rowHeight})` : 0}}
			data-scroll-anchor-key={`dm-${channel.id}`}
			data-pending-removal={pendingRemoval ? 'true' : undefined}
			data-last-row={isLast ? 'true' : undefined}
			data-flx="app.guild-list-dm-animation.dm-list-row"
		>
			<div
				ref={itemRef}
				className={styles.dmListItemBox}
				style={{
					transform: pendingRemoval ? guildRailDMClosedTransform(rtl) : 'translateX(0px)',
				}}
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
			</div>
		</div>
	);
};

export function useFrameBatchedDMListRows(rows: ReadonlyArray<DMListRow>, paused = false): ReadonlyArray<DMListRow> {
	const [batchedRows, setBatchedRows] = useState<ReadonlyArray<DMListRow>>(rows);
	const latestRowsRef = useRef(rows);
	const pendingRowsRef = useRef(rows);
	const batchAnimationFrameRef = useRef<number | null>(null);
	latestRowsRef.current = rows;
	useEffect(() => {
		if (paused) {
			if (batchAnimationFrameRef.current != null) cancelAnimationFrame(batchAnimationFrameRef.current);
			batchAnimationFrameRef.current = null;
			return;
		}
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
	}, [rows, paused]);
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
