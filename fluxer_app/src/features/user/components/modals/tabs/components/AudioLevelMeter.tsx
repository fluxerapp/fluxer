// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/user/components/modals/tabs/components/AudioLevelMeter.module.css';
import {clsx} from 'clsx';
import type {HTMLAttributes} from 'react';
import {useMemo} from 'react';

interface AudioLevelMeterProps {
	level: number;
}

const BAR_COUNT = 20;

type AudioLevelBarStyle = {
	'--bar-top': string;
	'--bar-bottom': string;
};

function makeActiveBarStyle(index: number): AudioLevelBarStyle {
	const positionBias = index / Math.max(1, BAR_COUNT - 1);
	const hue = 124 - positionBias * 124;
	const topLightness = 62 - positionBias * 8;
	const bottomLightness = 42 - positionBias * 10;
	return {
		'--bar-top': `hsl(${hue} 94% ${topLightness}%)`,
		'--bar-bottom': `hsl(${Math.max(0, hue - 8)} 90% ${bottomLightness}%)`,
	};
}

export function AudioLevelMeter({level, className, ...rest}: AudioLevelMeterProps & HTMLAttributes<HTMLDivElement>) {
	const bars = useMemo(() => {
		const clampedLevel = Math.min(1, Math.max(0, level));
		return Array.from({length: BAR_COUNT}, (_, index) => {
			const normalized = (index + 1) / BAR_COUNT;
			const active = clampedLevel >= normalized;
			return {
				active,
				height: `${Math.max(22, Math.round(28 + normalized * 72))}%`,
				style: active ? makeActiveBarStyle(index) : undefined,
			};
		});
	}, [level]);
	return (
		<div className={clsx(styles.track, className)} data-flx="user.audio-level-meter.track" {...rest}>
			{bars.map((bar, index) => (
				<span
					key={index}
					className={styles.bar}
					data-active={bar.active ? 'true' : 'false'}
					style={{height: bar.height, ...bar.style}}
					data-flx={`user.audio-level-meter.bar--${index}`}
				/>
			))}
		</div>
	);
}
