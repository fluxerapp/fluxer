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

import {memo} from 'react';
import {StatusTypes} from '~/Constants';

interface StatusIndicatorProps {
	status: string;
	size?: number;
	className?: string;
	appearance?: 'default' | 'monochrome';
	monochromeColor?: string;
}

const normalizeStatus = (status: string) => (status === StatusTypes.INVISIBLE ? StatusTypes.OFFLINE : status);

export const StatusIndicator = memo(
	({status, size = 12, className, appearance = 'default', monochromeColor}: StatusIndicatorProps) => {
		const normalizedStatus = normalizeStatus(status);
		const maskId = `svg-mask-status-${normalizedStatus}`;
		const fill =
			appearance === 'monochrome' ? (monochromeColor ?? 'currentColor') : `var(--status-${normalizedStatus})`;

		return (
			<svg
				className={className}
				width={size}
				height={size}
				viewBox="0 0 1 1"
				preserveAspectRatio="none"
				style={{display: 'block'}}
				aria-hidden={false}
				aria-label={`status-${normalizedStatus}`}
				role="img"
			>
				<rect x={0} y={0} width={1} height={1} fill={fill} mask={`url(#${maskId})`} />
			</svg>
		);
	},
);

interface RenderStatusIconOptions {
	appearance?: 'default' | 'monochrome';
	monochromeColor?: string;
}

export const renderStatusIconContent = (status: string, size: number, options: RenderStatusIconOptions = {}) => {
	const {appearance = 'default', monochromeColor} = options;
	const normalizedStatus = normalizeStatus(status);
	const maskId = `svg-mask-status-${normalizedStatus}`;
	const fill = appearance === 'monochrome' ? (monochromeColor ?? 'currentColor') : `var(--status-${normalizedStatus})`;

	return (
		// biome-ignore lint/a11y/noSvgWithoutTitle: decorative SVG, aria-hidden
		<svg width={size} height={size} viewBox="0 0 1 1" preserveAspectRatio="none" style={{display: 'block'}} aria-hidden>
			<rect x={0} y={0} width={1} height={1} fill={fill} mask={`url(#${maskId})`} />
		</svg>
	);
};
