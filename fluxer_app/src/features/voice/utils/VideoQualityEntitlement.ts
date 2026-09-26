// SPDX-License-Identifier: AGPL-3.0-or-later

import {LimitResolver} from '@app/features/app/utils/LimitResolverAdapter';
import {isLimitToggleEnabled} from '@app/features/app/utils/LimitUtils';

export function hasHigherVideoQuality(): boolean {
	return isLimitToggleEnabled(
		{
			feature_higher_video_quality: LimitResolver.resolve({
				key: 'feature_higher_video_quality',
				fallback: 0,
			}),
		},
		'feature_higher_video_quality',
	);
}
