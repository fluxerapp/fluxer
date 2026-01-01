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

import React from 'react';
import type {MentionSegment} from '~/utils/TextareaSegmentManager';
import {TextareaSegmentManager} from '~/utils/TextareaSegmentManager';

interface UseTextareaSegmentsReturn {
	segmentManagerRef: React.MutableRefObject<TextareaSegmentManager>;
	previousValueRef: React.MutableRefObject<string>;
	displayToActual: (displayText: string) => string;
	insertSegment: (
		currentText: string,
		insertPosition: number,
		displayText: string,
		actualText: string,
		type: MentionSegment['type'],
		id: string,
	) => {newText: string; newSegments: Array<MentionSegment>};
	handleTextChange: (newValue: string, oldValue: string) => void;
	clearSegments: () => void;
}

export function useTextareaSegments(): UseTextareaSegmentsReturn {
	const segmentManagerRef = React.useRef(new TextareaSegmentManager());
	const previousValueRef = React.useRef('');

	const displayToActual = React.useCallback((displayText: string): string => {
		return segmentManagerRef.current.displayToActual(displayText);
	}, []);

	const insertSegment = React.useCallback(
		(
			currentText: string,
			insertPosition: number,
			displayText: string,
			actualText: string,
			type: MentionSegment['type'],
			id: string,
		) => {
			return segmentManagerRef.current.insertSegment(currentText, insertPosition, displayText, actualText, type, id);
		},
		[],
	);

	const handleTextChange = React.useCallback((newValue: string, oldValue: string) => {
		const {changeStart, changeEnd, replacementLength} = TextareaSegmentManager.detectChange(oldValue, newValue);
		segmentManagerRef.current.updateSegmentsForTextChange(changeStart, changeEnd, replacementLength);
		previousValueRef.current = newValue;
	}, []);

	const clearSegments = React.useCallback(() => {
		segmentManagerRef.current.clear();
		previousValueRef.current = '';
	}, []);

	return {
		segmentManagerRef,
		previousValueRef,
		displayToActual,
		insertSegment,
		handleTextChange,
		clearSegments,
	};
}
