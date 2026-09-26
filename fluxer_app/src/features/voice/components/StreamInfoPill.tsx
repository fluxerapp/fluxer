// SPDX-License-Identifier: AGPL-3.0-or-later

import {LiveBadge} from '@app/features/ui/components/LiveBadge';
import type {TooltipPosition} from '@app/features/ui/tooltip/Tooltip';
import styles from '@app/features/voice/components/StreamInfoPill.module.css';
import type {StreamTrackInfo} from '@app/features/voice/components/useStreamTrackInfo';
import type {ScreenShareTarget} from '@app/features/voice/utils/ScreenShareOptions';
import {formatScreenShareTargetLabel} from '@app/features/voice/utils/VoiceMessageDescriptors';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {useMemo} from 'react';

const RESOLUTION_WITH_FPS_DESCRIPTOR = msg({
	message: '{resolution} {fps} FPS',
	comment:
		'Compact stream info pill label. {resolution} is a technical token such as 1080p or 4K. {fps} is the frame rate. FPS is a technical token.',
});

type ResolutionHeight = 240 | 480 | 720 | 1080 | 1440 | 2160;

const RESOLUTION_HEIGHTS: Array<ResolutionHeight> = [480, 240, 720, 1080, 1440, 2160];

const RESOLUTION_LABELS: Record<ResolutionHeight, string> = {
	240: '240p',
	480: '480p',
	720: '720p',
	1080: '1080p',
	1440: '1440p',
	2160: '4K',
};

type StreamInfoPillTone = 'default' | 'voice_tile';

export type StreamInfoPillQuality = StreamTrackInfo | {target: ScreenShareTarget};

function getClosestResolutionHeight(height: number) {
	let closest: ResolutionHeight = RESOLUTION_HEIGHTS[0];
	let smallestDiff = Math.abs(height - closest);
	for (const value of RESOLUTION_HEIGHTS) {
		const diff = Math.abs(height - value);
		if (diff < smallestDiff) {
			closest = value;
			smallestDiff = diff;
		}
	}
	return closest;
}

interface StreamInfoPillProps {
	info: StreamInfoPillQuality;
	className?: string;
	showLiveBadge?: boolean;
	tone?: StreamInfoPillTone;
	liveBadgeTooltipPosition?: TooltipPosition;
}

export function StreamInfoPill({
	info,
	className,
	showLiveBadge = true,
	tone = 'default',
	liveBadgeTooltipPosition,
}: StreamInfoPillProps) {
	const {i18n} = useLingui();
	const labelText = useMemo(() => {
		if ('target' in info) {
			return i18n._(RESOLUTION_WITH_FPS_DESCRIPTOR, {
				resolution: formatScreenShareTargetLabel(i18n, info.target),
				fps: i18n.number(info.target.frameRate),
			});
		}
		const resolutionText = RESOLUTION_LABELS[getClosestResolutionHeight(info.height)];
		if (!Number.isFinite(info.fps) || info.fps <= 0) return resolutionText;
		return i18n._(RESOLUTION_WITH_FPS_DESCRIPTOR, {resolution: resolutionText, fps: i18n.number(info.fps)});
	}, [info, i18n.locale]);
	return (
		<div
			className={clsx(styles.container, tone === 'voice_tile' && styles.containerOnTile, className)}
			data-flx="voice.stream-info-pill.container"
		>
			<span
				className={clsx(styles.pill, tone === 'voice_tile' && styles.pillOnTile)}
				data-flx="voice.stream-info-pill.pill"
			>
				{labelText}
			</span>
			{showLiveBadge && (
				<LiveBadge
					tone={tone}
					tooltipPosition={liveBadgeTooltipPosition}
					data-flx="voice.stream-info-pill.live-badge"
				/>
			)}
		</div>
	);
}
