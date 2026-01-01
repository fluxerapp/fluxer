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
			browsers: 'last 10 years, > 0.5%, not dead',
		}),
		autoprefixer({
			flexbox: 'no-2009',
			grid: 'no-autoplace',
		}),
	],
};
