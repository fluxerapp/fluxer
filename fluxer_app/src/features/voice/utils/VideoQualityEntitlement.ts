// SPDX-License-Identifier: AGPL-3.0-or-later

import {LimitResolver} from '@app/features/app/utils/LimitResolverAdapter';
import {isLimitToggleEnabled} from '@app/features/app/utils/LimitUtils';
import {isActiveVoiceChannelP2p} from '@app/features/voice/state/ChannelP2pStatus';
import {MAX_SCREEN_SHARE_BITRATE} from '@fluxer/constants/src/LimitConstants';

export function hasHigherVideoQuality(): boolean {
	return (
		isActiveVoiceChannelP2p() ||
		isLimitToggleEnabled(
			{
				feature_higher_video_quality: LimitResolver.resolve({
					key: 'feature_higher_video_quality',
					fallback: 0,
				}),
			},
			'feature_higher_video_quality',
		)
	);
}

export function getScreenShareBitrateCeilingBps(): number {
	return LimitResolver.resolve({key: 'max_screen_share_bitrate', fallback: MAX_SCREEN_SHARE_BITRATE});
}
