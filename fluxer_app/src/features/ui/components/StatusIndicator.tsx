// SPDX-License-Identifier: AGPL-3.0-or-later

import {getStatusTypeLabel} from '@app/features/app/constants/AppConstants';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import styles from '@app/features/ui/components/StatusIndicator.module.css';
import {StatusTypes} from '@fluxer/constants/src/StatusConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import clsx from 'clsx';
import {memo} from 'react';

const STATUS_DESCRIPTOR = msg({message: '{effectiveStatusLabel} status'});

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
		const {i18n} = useLingui();
		const normalizedStatus = normalizeStatus(status);
		const effectiveStatusLabel = getStatusTypeLabel(i18n, normalizedStatus);
		const maskId = `flx-mask-presence-${normalizedStatus}`;
		const fill =
			appearance === 'monochrome' ? (monochromeColor ?? 'currentColor') : `var(--status-${normalizedStatus})`;
		return (
			<svg
				className={clsx(className, styles.displayBlock)}
				width={remFromPx(size)}
				height={remFromPx(size)}
				viewBox="0 0 1 1"
				preserveAspectRatio="none"
				aria-hidden={false}
				aria-label={i18n._(STATUS_DESCRIPTOR, {effectiveStatusLabel})}
				role="img"
				data-flx="ui.status-indicator.display-block"
			>
				<rect
					x={0}
					y={0}
					width={1}
					height={1}
					fill={fill}
					mask={`url(#${maskId})`}
					data-flx="ui.status-indicator.rect"
				/>
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
	const maskId = `flx-mask-presence-${normalizedStatus}`;
	const fill = appearance === 'monochrome' ? (monochromeColor ?? 'currentColor') : `var(--status-${normalizedStatus})`;
	return (
		<svg
			width={remFromPx(size)}
			height={remFromPx(size)}
			viewBox="0 0 1 1"
			preserveAspectRatio="none"
			className={styles.displayBlock}
			aria-hidden
			data-flx="ui.status-indicator.render-status-icon-content.display-block"
		>
			<rect
				x={0}
				y={0}
				width={1}
				height={1}
				fill={fill}
				mask={`url(#${maskId})`}
				data-flx="ui.status-indicator.render-status-icon-content.rect"
			/>
		</svg>
	);
};
