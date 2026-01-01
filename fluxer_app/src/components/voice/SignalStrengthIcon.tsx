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

import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {useEffect, useMemo, useState} from 'react';
import styles from './SignalStrengthIcon.module.css';

interface Props {
	latency: number | null;
	className?: string;
	thresholds?: [number, number, number];
	size?: number;
	strokeWidth?: number;
}

const ARC_COUNT = 4;
const LOADING_STEP_MS = 280;

function useInterval(enabled: boolean, callback: () => void, delayMs: number) {
	useEffect(() => {
		if (!enabled) return;
		const id = window.setInterval(callback, delayMs);
		return () => window.clearInterval(id);
	}, [enabled, callback, delayMs]);
}

interface LatencyLoadingState {
	kind: 'loading';
}

interface LatencyValueState {
	kind: 'value';
	filledCount: number;
	colorClass: string;
}

type LatencyState = LatencyLoadingState | LatencyValueState;

function getLatencyState(latency: number | null, thresholds: [number, number, number]): LatencyState {
	if (latency === null) return {kind: 'loading'};

	const [good, ok, meh] = thresholds;

	switch (true) {
		case latency < good:
			return {kind: 'value', filledCount: 4, colorClass: styles.green};
		case latency < ok:
			return {kind: 'value', filledCount: 3, colorClass: styles.yellow};
		case latency < meh:
			return {kind: 'value', filledCount: 2, colorClass: styles.orange};
		default:
			return {kind: 'value', filledCount: 1, colorClass: styles.red};
	}
}

export function SignalStrengthIcon({
	latency,
	className,
	thresholds = [50, 100, 150],
	size = 16,
	strokeWidth = 2,
}: Props) {
	const {t} = useLingui();
	const [loadingIndex, setLoadingIndex] = useState(0);

	useInterval(
		latency === null,
		() => {
			const total = ARC_COUNT + 1;
			setLoadingIndex((prev) => (prev + 1) % total);
		},
		LOADING_STEP_MS,
	);

	const state = useMemo(() => getLatencyState(latency, thresholds), [latency, thresholds]);

	const geom = useMemo(() => {
		const viewSize = size;
		const ox = strokeWidth / 2;
		const oy = viewSize - strokeWidth / 2;
		const maxR = viewSize - strokeWidth;
		const step = maxR / ARC_COUNT;
		const dotR = Math.max(1.5, strokeWidth - 0.5);

		const arcs = Array.from({length: ARC_COUNT}, (_, i) => {
			const r = step * (i + 1);
			const sx = ox;
			const sy = oy - r;
			const ex = ox + r;
			const ey = oy;
			const d = `M ${sx} ${sy} A ${r} ${r} 0 0 1 ${ex} ${ey}`;
			return {r, d, arcIndex: i + 1};
		});

		return {viewSize, ox, oy, dotR, arcs};
	}, [size, strokeWidth]);

	const ariaLabel = latency === null ? t`Signal strength loading` : t`Latency ${latency} milliseconds`;

	const dotClass = useMemo(() => {
		if (state.kind === 'loading') {
			return clsx(loadingIndex === 0 ? styles.tertiary : styles.tertiaryMuted);
		}
		return state.colorClass;
	}, [state, loadingIndex]);

	return (
		<svg
			width={geom.viewSize}
			height={geom.viewSize}
			viewBox={`0 0 ${geom.viewSize} ${geom.viewSize}`}
			className={clsx(styles.svg, className)}
			role="img"
			aria-label={ariaLabel}
		>
			<circle cx={geom.ox} cy={geom.oy} r={geom.dotR} className={dotClass} fill="currentColor" />

			{geom.arcs.map(({d, arcIndex}, i) => {
				const isActiveLoading = state.kind === 'loading' && loadingIndex === arcIndex;

				const isFilled = state.kind === 'value' ? i < state.filledCount : false;

				const arcClass =
					state.kind === 'loading'
						? clsx(isActiveLoading ? styles.tertiary : styles.tertiaryMuted)
						: clsx(isFilled ? state.colorClass : styles.tertiaryMuted);

				return (
					<path
						key={arcIndex}
						d={d}
						fill="none"
						stroke="currentColor"
						strokeWidth={strokeWidth}
						className={arcClass}
						strokeLinecap="round"
					/>
				);
			})}
		</svg>
	);
}
