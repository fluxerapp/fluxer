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

import {useMemo} from 'react';

export function generatePlaceholderSpecs(options: {
	compact: boolean;
	messageGroups: number;
	groupRange: number;
	attachments: number;
	fontSize: number;
	groupSpacing: number;
}): {
	messages: Array<number>;
	attachmentSpecs: Array<[number, {width: number; height: number}] | undefined>;
	totalHeight: number;
	groupSpacing: number;
} {
	const {compact, messageGroups, groupRange, attachments, fontSize, groupSpacing} = options;

	if (attachments > messageGroups) {
		throw new Error(
			`generatePlaceholderSpecs: too many attachments relative to messageGroups: ${messageGroups}, ${attachments}`,
		);
	}

	const DEFAULT_FONT_SIZE = 16;
	const scale = fontSize / DEFAULT_FONT_SIZE;

	const MESSAGE_HEIGHT_COZY = 22;
	const MESSAGE_HEIGHT_COMPACT = 16;
	const ATTACHMENT_MARGIN = 8;

	const messageHeight = compact ? MESSAGE_HEIGHT_COMPACT : MESSAGE_HEIGHT_COZY;

	let totalHeight = 0;
	const messageCounts: Array<number> = [];

	for (let i = 0; i < messageGroups; i++) {
		const count = Math.floor(Math.random() * groupRange) + 1;
		messageCounts.push(count);

		totalHeight += groupSpacing * scale;
		totalHeight += messageHeight * scale;
		totalHeight += (count - 1) * messageHeight * scale;
	}

	const availableGroupIndices = messageCounts.map((_, i) => i);
	const attachmentSpecs: Array<[number, {width: number; height: number}] | undefined> =
		Array(messageGroups).fill(undefined);

	for (let i = 0; i < attachments; i++) {
		const randomIndex = Math.floor(Math.random() * availableGroupIndices.length);
		const groupIndex = availableGroupIndices.splice(randomIndex, 1)[0];

		const width = Math.floor(Math.random() * (400 - 140 + 1)) + 140;
		const height = Math.floor(Math.random() * (320 - 100 + 1)) + 100;

		attachmentSpecs[groupIndex] = [groupIndex, {width, height}];
		totalHeight += height + ATTACHMENT_MARGIN * scale;
	}

	return {
		messages: messageCounts,
		attachmentSpecs,
		totalHeight,
		groupSpacing,
	};
}

export function usePlaceholderSpecs(compact: boolean, groupSpacing: number, fontSize: number) {
	return useMemo(() => {
		return compact
			? generatePlaceholderSpecs({
					compact: true,
					messageGroups: 30,
					groupRange: 4,
					attachments: 8,
					fontSize,
					groupSpacing,
				})
			: generatePlaceholderSpecs({
					compact: false,
					messageGroups: 26,
					groupRange: 4,
					attachments: 8,
					fontSize,
					groupSpacing,
				});
	}, [compact, fontSize, groupSpacing]);
}
