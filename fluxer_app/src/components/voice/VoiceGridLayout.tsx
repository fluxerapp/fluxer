/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {TrackReferenceOrPlaceholder} from '@livekit/components-react';
import {isTrackReference, ParticipantContext, TrackRefContext} from '@livekit/components-react';
import type React from 'react';
import {useEffect, useRef, useState} from 'react';

interface VoiceGridLayoutProps {
	tracks: Array<TrackReferenceOrPlaceholder>;
	children: React.ReactElement;
}

interface GridDimensions {
	tileWidth: number;
	tileHeight: number;
	cols: number;
	rows: number;
}

function calculateGridDimensions(
	containerWidth: number,
	containerHeight: number,
	tileCount: number,
	gap: number = 12,
): GridDimensions {
	if (tileCount === 0) {
		return {tileWidth: 0, tileHeight: 0, cols: 1, rows: 1};
	}

	if (tileCount === 1) {
		const containerAspect = containerWidth / containerHeight;
		const targetAspect = 16 / 9;

		let width: number;
		let height: number;

		if (containerAspect > targetAspect) {
			height = containerHeight;
			width = height * targetAspect;
		} else {
			width = containerWidth;
			height = width / targetAspect;
		}

		return {
			tileWidth: width,
			tileHeight: height,
			cols: 1,
			rows: 1,
		};
	}

	if (tileCount === 2) {
		const containerAspect = containerWidth / containerHeight;
		const ultraWideThreshold = 32 / 9;

		if (containerAspect > ultraWideThreshold) {
			return calculateOptimalTileSize(containerWidth, containerHeight, 2, 1, gap);
		} else {
			return calculateOptimalTileSize(containerWidth, containerHeight, 1, 2, gap);
		}
	}

	let bestDimensions: GridDimensions | null = null;
	let maxTileSize = 0;

	for (let cols = 1; cols <= tileCount; cols++) {
		const rows = Math.ceil(tileCount / cols);

		const dimensions = calculateOptimalTileSize(containerWidth, containerHeight, cols, rows, gap);
		const tileSize = dimensions.tileWidth * dimensions.tileHeight;

		if (tileSize > maxTileSize) {
			maxTileSize = tileSize;
			bestDimensions = dimensions;
		}
	}

	return bestDimensions || {tileWidth: 0, tileHeight: 0, cols: 1, rows: 1};
}

function calculateOptimalTileSize(
	containerWidth: number,
	containerHeight: number,
	cols: number,
	rows: number,
	gap: number,
): GridDimensions {
	const availableWidth = containerWidth - gap * (cols - 1);
	const availableHeight = containerHeight - gap * (rows - 1);

	let tileWidth = availableWidth / cols;
	let tileHeight = tileWidth * (9 / 16);

	const totalHeight = tileHeight * rows + gap * (rows - 1);

	if (totalHeight > containerHeight) {
		tileHeight = availableHeight / rows;
		tileWidth = tileHeight * (16 / 9);
	}

	return {
		tileWidth,
		tileHeight,
		cols,
		rows,
	};
}

export function VoiceGridLayout({tracks, children}: VoiceGridLayoutProps) {
	const containerRef = useRef<HTMLDivElement>(null);
	const [dimensions, setDimensions] = useState<GridDimensions>({
		tileWidth: 0,
		tileHeight: 0,
		cols: 1,
		rows: 1,
	});

	useEffect(() => {
		if (typeof ResizeObserver === 'undefined') return;

		const container = containerRef.current;
		if (!container) return;

		const updateDimensions = () => {
			const rect = container.getBoundingClientRect();
			const tileCount = tracks.length;
			const newDimensions = calculateGridDimensions(rect.width, rect.height, tileCount);
			setDimensions(newDimensions);
		};

		updateDimensions();

		const resizeObserver = new ResizeObserver(() => {
			updateDimensions();
		});

		resizeObserver.observe(container);

		return () => {
			resizeObserver.disconnect();
		};
	}, [tracks.length]);

	return (
		<div
			ref={containerRef}
			style={{
				width: '100%',
				height: '100%',
				display: 'flex',
				alignItems: 'center',
				justifyContent: 'center',
				position: 'relative',
			}}
		>
			<div
				style={{
					display: 'grid',
					gridTemplateColumns: `repeat(${dimensions.cols}, ${dimensions.tileWidth}px)`,
					gridTemplateRows: `repeat(${dimensions.rows}, ${dimensions.tileHeight}px)`,
					gap: '12px',
					justifyContent: 'center',
					alignContent: 'center',
				}}
			>
				{tracks.map((trackRef, index) => {
					const key = isTrackReference(trackRef)
						? `${trackRef.participant.identity}-${trackRef.source}`
						: `placeholder-${trackRef.participant.identity}-${index}`;

					return (
						<TrackRefContext.Provider key={key} value={trackRef}>
							<ParticipantContext.Provider value={trackRef.participant}>{children}</ParticipantContext.Provider>
						</TrackRefContext.Provider>
					);
				})}
			</div>
		</div>
	);
}
