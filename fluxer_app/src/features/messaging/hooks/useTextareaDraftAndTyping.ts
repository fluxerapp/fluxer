// SPDX-License-Identifier: AGPL-3.0-or-later

import * as DraftCommands from '@app/features/messaging/commands/DraftCommands';
import {segmentsEqual} from '@app/features/messaging/state/MessagingDrafts';
import type {MentionSegment, TextareaSegmentManager} from '@app/features/messaging/utils/TextareaSegmentManager';
import {onBeforeAppStorageScopeChange} from '@app/features/platform/state/PersistentStorage';
import {flushPendingPersistWrites} from '@app/features/platform/utils/MobXPersistence';
import {TypingUtils} from '@app/features/typing/utils/TypingUtils';
import {useEffect, useRef} from 'react';

interface UseTextareaDraftAndTypingOptions {
	draftOwner: DraftCommands.DraftOwner;
	channelId: string;
	value: string;
	setValue: React.Dispatch<React.SetStateAction<string>>;
	draft: string | null;
	draftSegments?: ReadonlyArray<MentionSegment> | null;
	previousValueRef: React.RefObject<string>;
	segmentManagerRef?: React.RefObject<TextareaSegmentManager>;
	enabled: boolean;
	typingEnabled?: boolean;
	isEditingMessageInComposer: boolean;
}

function cloneSegments(segments: ReadonlyArray<MentionSegment> | null | undefined): Array<MentionSegment> {
	return segments?.map((segment) => ({...segment})) ?? [];
}

export const useTextareaDraftAndTyping = ({
	draftOwner,
	channelId,
	value,
	setValue,
	draft,
	draftSegments,
	previousValueRef,
	segmentManagerRef,
	enabled,
	typingEnabled = enabled,
	isEditingMessageInComposer,
}: UseTextareaDraftAndTypingOptions) => {
	const isRestoringDraftRef = useRef(false);
	const isEditingMessageInComposerRef = useRef(isEditingMessageInComposer);
	const typingPreviousValueRef = useRef<{channelId: string; value: string} | null>(null);
	const currentDraftRef = useRef(draft);
	const currentDraftSegmentsRef = useRef<ReadonlyArray<MentionSegment>>(draftSegments ?? []);
	const pendingDraftRef = useRef<{
		channelId: string;
		value: string;
		segments: Array<MentionSegment> | null;
	} | null>(null);
	useEffect(() => {
		currentDraftRef.current = draft;
		currentDraftSegmentsRef.current = draftSegments ?? [];
	}, [draft, draftSegments]);
	useEffect(() => {
		isEditingMessageInComposerRef.current = isEditingMessageInComposer;
	}, [isEditingMessageInComposer]);
	useEffect(() => {
		if (enabled) {
			return;
		}
		TypingUtils.clear(channelId);
		pendingDraftRef.current = null;
		if (currentDraftRef.current) {
			DraftCommands.deleteDraft(draftOwner, channelId);
		}
		segmentManagerRef?.current.clear();
		isRestoringDraftRef.current = true;
		setValue('');
		if (previousValueRef.current !== undefined && previousValueRef.current !== null) {
			previousValueRef.current = '';
		}
		const timer = setTimeout(() => {
			isRestoringDraftRef.current = false;
		}, 0);
		return () => {
			clearTimeout(timer);
			isRestoringDraftRef.current = false;
		};
	}, [draftOwner, channelId, enabled, setValue, previousValueRef]);
	useEffect(() => {
		if (isEditingMessageInComposer) {
			return;
		}
		if (!draft || previousValueRef.current === undefined || !DraftCommands.ownsActiveDrafts(draftOwner)) {
			return;
		}
		isRestoringDraftRef.current = true;
		segmentManagerRef?.current.setSegments(cloneSegments(draftSegments));
		if (draft !== value) {
			setValue(draft);
		}
		if (previousValueRef.current !== null) {
			previousValueRef.current = draft;
		}
		const timer = setTimeout(() => {
			isRestoringDraftRef.current = false;
		}, 0);
		return () => {
			clearTimeout(timer);
			isRestoringDraftRef.current = false;
		};
	}, [draftOwner, draft, draftSegments, previousValueRef, setValue, isEditingMessageInComposer]);
	const flushDraftRef = useRef<() => void>(() => {});
	useEffect(() => {
		const flush = () => {
			const pending = pendingDraftRef.current;
			if (!pending) return;
			pendingDraftRef.current = null;
			const currentDraft = currentDraftRef.current ?? '';
			const currentSegments = currentDraftSegmentsRef.current ?? [];
			if (
				pending.value === currentDraft &&
				(pending.segments === null || segmentsEqual(pending.segments, currentSegments))
			) {
				return;
			}
			if (pending.value) {
				if (pending.segments === null) {
					DraftCommands.createDraft(draftOwner, pending.channelId, pending.value);
				} else {
					DraftCommands.createDraft(draftOwner, pending.channelId, pending.value, pending.segments);
				}
			} else {
				DraftCommands.deleteDraft(draftOwner, pending.channelId);
			}
		};
		flushDraftRef.current = flush;
		if (isEditingMessageInComposer || isRestoringDraftRef.current || !DraftCommands.ownsActiveDrafts(draftOwner)) {
			return;
		}
		pendingDraftRef.current = {
			channelId,
			value,
			segments: segmentManagerRef ? cloneSegments(segmentManagerRef.current.getSegments()) : null,
		};
		const timer = setTimeout(flush, 400);
		return () => {
			clearTimeout(timer);
		};
	}, [draftOwner, channelId, value, isEditingMessageInComposer]);
	useEffect(() => {
		return () => {
			flushDraftRef.current();
		};
	}, [channelId, isEditingMessageInComposer]);
	useEffect(() => {
		const flushBeforeUnload = (): void => {
			flushDraftRef.current();
			flushPendingPersistWrites();
		};
		const stopScopeChange = onBeforeAppStorageScopeChange(flushBeforeUnload);
		window.addEventListener('pagehide', flushBeforeUnload);
		window.addEventListener('beforeunload', flushBeforeUnload);
		return () => {
			stopScopeChange();
			window.removeEventListener('pagehide', flushBeforeUnload);
			window.removeEventListener('beforeunload', flushBeforeUnload);
		};
	}, []);
	useEffect(() => {
		const typingPreviousValue = typingPreviousValueRef.current;
		typingPreviousValueRef.current = {channelId, value};
		if (isRestoringDraftRef.current) {
			return;
		}
		TypingUtils.handleComposerChange({
			channelId,
			value,
			previousValue: typingPreviousValue?.channelId === channelId ? typingPreviousValue.value : null,
			enabled,
			typingEnabled,
			isEditingMessageInComposer: isEditingMessageInComposerRef.current,
		});
	}, [channelId, value, enabled, typingEnabled]);
};
