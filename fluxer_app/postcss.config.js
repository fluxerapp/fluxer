// SPDX-License-Identifier: AGPL-3.0-or-later

import autoprefixer from 'autoprefixer';
import postcssDiscardComments from 'postcss-discard-comments';
import postcssPresetEnv from 'postcss-preset-env';

export default {
	plugins: [
		postcssDiscardComments({
			removeAll: true,
		}),
		postcssPresetEnv({
			stage: 3,
			features: {
				'nesting-rules': true,
				'custom-properties': true,
				'custom-media-queries': true,
			},
		}),
		autoprefixer({
			flexbox: 'no-2009',
			grid: 'no-autoplace',
		}),
	],
};
