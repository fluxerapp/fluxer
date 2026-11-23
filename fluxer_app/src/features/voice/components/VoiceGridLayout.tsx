// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/voice/components/VoiceGridLayout.module.css';
import {
	getVoiceGridRowStyle,
	resolveVoiceGridPackedLayoutMetrics,
	type VoiceGridRowStyle,
} from '@app/features/voice/components/VoiceGridLayoutMetrics';
import type {VoiceGridEntry} from '@app/features/voice/components/VoiceParticipantConsolidation';
import {VoiceTileGroupContext} from '@app/features/voice/components/VoiceTileGroupContext';
import {ParticipantContext, TrackRefContext} from '@livekit/components-react';
import {clsx} from 'clsx';
import type React from 'react';
import {useEffect, useLayoutEffect, useMemo, useRef, useState} from 'react';

interface VoiceGridLayoutProps {
	entries: Array<VoiceGridEntry>;
	children: React.ReactElement;
	edgeToEdge?: boolean;
	compact?: boolean;
	onCapacityChange?: (info: VoiceGridCapacityInfo) => void;
	onExpandUser: (userId: string) => void;
}

export interface VoiceGridCapacityInfo {
	visibleTileCount: number;
	totalTileCount: number;
	overflow: boolean;
	viewportWidth: number;
	viewportHeight: number;
}

type GridStyle = React.CSSProperties &
	VoiceGridRowStyle & {
		'--voice-grid-columns'?: string;
		'--voice-grid-rows'?: string;
	};

interface VoiceGridViewportSize {
	width: number;
	height: number;
}

function roundViewportSize(value: number): number {
	if (!Number.isFinite(value)) return 0;
	return Math.max(0, Math.round(value));
}

function readResizeObserverSize(entry: ResizeObserverEntry): VoiceGridViewportSize {
	return {
		width: roundViewportSize(entry.contentRect.width),
		height: roundViewportSize(entry.contentRect.height),
	};
}

function readElementSize(element: HTMLElement): VoiceGridViewportSize {
	return {
		width: roundViewportSize(element.clientWidth),
		height: roundViewportSize(element.clientHeight),
	};
}

export function VoiceGridLayout({
	entries,
	children,
	edgeToEdge = false,
	compact = false,
	onCapacityChange,
	onExpandUser,
}: VoiceGridLayoutProps) {
	const viewportRef = useRef<HTMLDivElement | null>(null);
	const [viewportSize, setViewportSize] = useState<VoiceGridViewportSize | null>(null);
	useLayoutEffect(() => {
		const viewportNode = viewportRef.current;
		if (!viewportNode) return;
		const ownerWindow = viewportNode.ownerDocument.defaultView ?? window;
		const commitSize = (nextSize: VoiceGridViewportSize) => {
			setViewportSize((previousSize) => {
				if (previousSize?.width === nextSize.width && previousSize.height === nextSize.height) return previousSize;
				return nextSize;
			});
		};
		commitSize(readElementSize(viewportNode));
		if (typeof ownerWindow.ResizeObserver === 'undefined') {
			const handleResize = () => commitSize(readElementSize(viewportNode));
			ownerWindow.addEventListener('resize', handleResize);
			return () => ownerWindow.removeEventListener('resize', handleResize);
		}
		const resizeObserver = new ownerWindow.ResizeObserver((entries) => {
			const entry = entries[0];
			if (!entry) return;
			commitSize(readResizeObserverSize(entry));
		});
		resizeObserver.observe(viewportNode);
		return () => resizeObserver.disconnect();
	}, []);
	const packedMetrics = useMemo(() => {
		if (!viewportSize) return entries.length;
		if (viewportSize.width === 0 || viewportSize.height === 0) return entries.length;
		return resolveVoiceGridPackedLayoutMetrics({
			tileCount: entries.length,
			containerWidth: viewportSize.width,
			containerHeight: viewportSize.height,
			compact,
			edgeToEdge,
		});
	}, [compact, edgeToEdge, entries.length, viewportSize]);
	const visibleTileCount = typeof packedMetrics === 'number' ? packedMetrics : packedMetrics.visibleTileCount;
	const visibleEntries = useMemo(() => entries.slice(0, visibleTileCount), [entries, visibleTileCount]);
	const hiddenTileCount = Math.max(0, entries.length - visibleEntries.length);
	useEffect(() => {
		if (!onCapacityChange) return;
		onCapacityChange({
			visibleTileCount,
			totalTileCount: entries.length,
			overflow: hiddenTileCount > 0,
			viewportWidth: viewportSize?.width ?? 0,
			viewportHeight: viewportSize?.height ?? 0,
		});
	}, [hiddenTileCount, onCapacityChange, entries.length, visibleTileCount, viewportSize]);
	const gridStyle = useMemo<GridStyle>(() => {
		const rowStyle = getVoiceGridRowStyle(visibleEntries.length);
		if (typeof packedMetrics === 'number') return rowStyle;
		return {
			...rowStyle,
			'--voice-grid-columns': `${packedMetrics.columns}`,
			'--voice-grid-rows': `${packedMetrics.rows}`,
		};
	}, [packedMetrics, visibleEntries.length]);
	return (
		<div
			ref={viewportRef}
			className={clsx(styles.gridViewport, compact && styles.gridViewportCompact)}
			data-edge-to-edge={edgeToEdge ? 'true' : undefined}
			data-flx="voice.voice-grid-layout.grid-viewport"
		>
			<div
				className={styles.grid}
				data-tile-count={visibleEntries.length}
				data-total-tile-count={entries.length}
				data-hidden-tile-count={hiddenTileCount > 0 ? hiddenTileCount : undefined}
				style={gridStyle}
				data-flx="voice.voice-grid-layout.grid"
			>
				{visibleEntries.map((entry) => {
					const groupValue = {
						hiddenConnectionCount: entry.hiddenConnectionCount,
						deviceConnectionCount: entry.deviceConnectionCount,
						isExpanded: entry.isDeviceGroupExpanded,
						isPrimary: Boolean(entry.isDeviceGroupPrimary),
						userId: entry.userId,
						onExpand: () => {
							if (entry.userId) onExpandUser(entry.userId);
						},
					};
					return (
						<div key={entry.key} className={styles.gridItem} data-flx="voice.voice-grid-layout.grid-item">
							<TrackRefContext.Provider value={entry.trackRef}>
								<ParticipantContext.Provider value={entry.trackRef.participant}>
									<VoiceTileGroupContext.Provider value={groupValue}>{children}</VoiceTileGroupContext.Provider>
								</ParticipantContext.Provider>
							</TrackRefContext.Provider>
						</div>
					);
				})}
			</div>
		</div>
	);
}
