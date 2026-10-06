// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MentionSegment, TextareaTextChangeHint} from '@app/features/messaging/utils/TextareaSegmentManager';
import {TextareaSegmentManager} from '@app/features/messaging/utils/TextareaSegmentManager';
import {useCallback, useRef} from 'react';

interface UseTextareaSegmentsReturn {
	segmentManagerRef: React.RefObject<TextareaSegmentManager>;
	previousValueRef: React.RefObject<string>;
	displayToActual: (displayText: string) => string;
	rememberSegmentsForValue: (value: string, segments?: ReadonlyArray<MentionSegment>) => void;
	prepareTextChange: (nextValue: string, nextSegments: ReadonlyArray<MentionSegment>) => void;
	insertSegment: (
		currentText: string,
		insertPosition: number,
		displayText: string,
		actualText: string,
		type: MentionSegment['type'],
		id: string,
	) => {
		newText: string;
		newSegments: Array<MentionSegment>;
	};
	handleTextChange: (newValue: string, oldValue: string, inputType?: string, hint?: TextareaTextChangeHint) => void;
	clearSegments: () => void;
}

const MAX_SEGMENT_SNAPSHOTS = 80;

function cloneSegments(segments: ReadonlyArray<MentionSegment>): Array<MentionSegment> {
	return segments.map((segment) => ({...segment}));
}

function areSegmentsValidForValue(value: string, segments: ReadonlyArray<MentionSegment>): boolean {
	for (const segment of segments) {
		if (segment.start < 0 || segment.end <= segment.start || segment.end > value.length) {
			return false;
		}
		if (value.slice(segment.start, segment.end) !== segment.displayText) {
			return false;
		}
	}
	return true;
}

function isHistoryInputType(inputType: string | undefined): boolean {
	return inputType === 'historyUndo' || inputType === 'historyRedo';
}

export function useTextareaSegments(): UseTextareaSegmentsReturn {
	const segmentManagerRef = useRef(new TextareaSegmentManager());
	const previousValueRef = useRef('');
	const segmentSnapshotsRef = useRef(new Map<string, Array<MentionSegment>>());
	const pendingTextChangeRef = useRef<{
		value: string;
		segments: Array<MentionSegment>;
	} | null>(null);
	const rememberSegmentsForValue = useCallback((value: string, segments?: ReadonlyArray<MentionSegment>) => {
		const snapshot = cloneSegments(segments ?? segmentManagerRef.current.getSegments());
		if (!areSegmentsValidForValue(value, snapshot)) {
			return;
		}
		const snapshots = segmentSnapshotsRef.current;
		snapshots.delete(value);
		snapshots.set(value, snapshot);
		while (snapshots.size > MAX_SEGMENT_SNAPSHOTS) {
			const oldestKey = snapshots.keys().next().value;
			if (oldestKey == null) break;
			snapshots.delete(oldestKey);
		}
	}, []);
	const prepareTextChange = useCallback(
		(nextValue: string, nextSegments: ReadonlyArray<MentionSegment>) => {
			rememberSegmentsForValue(previousValueRef.current);
			const clonedNextSegments = cloneSegments(nextSegments);
			rememberSegmentsForValue(nextValue, clonedNextSegments);
			pendingTextChangeRef.current = {
				value: nextValue,
				segments: clonedNextSegments,
			};
		},
		[rememberSegmentsForValue],
	);
	const displayToActual = useCallback((displayText: string): string => {
		return segmentManagerRef.current.displayToActual(displayText);
	}, []);
	const insertSegment = useCallback(
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
	const handleTextChange = useCallback(
		(newValue: string, oldValue: string, inputType?: string, hint?: TextareaTextChangeHint) => {
			const pending = pendingTextChangeRef.current;
			if (pending && pending.value === newValue) {
				segmentManagerRef.current.setSegments(cloneSegments(pending.segments));
				previousValueRef.current = newValue;
				rememberSegmentsForValue(newValue, pending.segments);
				pendingTextChangeRef.current = null;
				return;
			}
			if (pending) {
				pendingTextChangeRef.current = null;
			}
			const {changeStart, changeEnd, replacementLength} = TextareaSegmentManager.detectChange(oldValue, newValue, hint);
			segmentManagerRef.current.updateSegmentsForTextChange(changeStart, changeEnd, replacementLength);
			if (isHistoryInputType(inputType)) {
				const snapshot = segmentSnapshotsRef.current.get(newValue);
				if (snapshot && areSegmentsValidForValue(newValue, snapshot)) {
					segmentManagerRef.current.setSegments(cloneSegments(snapshot));
				}
			}
			previousValueRef.current = newValue;
			rememberSegmentsForValue(newValue);
		},
		[rememberSegmentsForValue],
	);
	const clearSegments = useCallback(() => {
		segmentManagerRef.current.clear();
		previousValueRef.current = '';
	}, []);
	return {
		segmentManagerRef,
		previousValueRef,
		displayToActual,
		rememberSegmentsForValue,
		prepareTextChange,
		insertSegment,
		handleTextChange,
		clearSegments,
	};
}
