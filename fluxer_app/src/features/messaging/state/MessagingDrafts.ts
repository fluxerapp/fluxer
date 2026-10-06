// SPDX-License-Identifier: AGPL-3.0-or-later

import TextareaSelection from '@app/features/messaging/state/TextareaSelection';
import type {MentionSegment} from '@app/features/messaging/utils/TextareaSegmentManager';
import {makePersistent} from '@app/features/platform/utils/MobXPersistence';
import {makeAutoObservable} from 'mobx';

const EMPTY_SEGMENTS: ReadonlyArray<MentionSegment> = [];

function cloneSegment(segment: MentionSegment): MentionSegment {
	return {...segment};
}

function normalizeDraftSegments(
	content: string,
	segments?: ReadonlyArray<MentionSegment> | null,
): Array<MentionSegment> {
	if (!segments || segments.length === 0) {
		return [];
	}
	return segments
		.filter((segment) => {
			if (segment.start < 0 || segment.end <= segment.start || segment.end > content.length) {
				return false;
			}
			return content.slice(segment.start, segment.end) === segment.displayText && segment.actualText.length > 0;
		})
		.map(cloneSegment)
		.sort((a, b) => a.start - b.start);
}

function segmentsEqual(a: ReadonlyArray<MentionSegment>, b: ReadonlyArray<MentionSegment>): boolean {
	if (a.length !== b.length) return false;
	for (let i = 0; i < a.length; i++) {
		const left = a[i];
		const right = b[i];
		if (
			left.type !== right.type ||
			left.id !== right.id ||
			left.displayText !== right.displayText ||
			left.actualText !== right.actualText ||
			left.start !== right.start ||
			left.end !== right.end
		) {
			return false;
		}
	}
	return true;
}

class Drafts {
	drafts: Record<string, string> = {};
	draftSegments: Record<string, Array<MentionSegment>> = {};

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		void this.initPersistence();
	}

	private async initPersistence(): Promise<void> {
		await makePersistent(this, 'Drafts', ['drafts', 'draftSegments']);
	}

	createDraft(channelId: string, content: string, segments?: ReadonlyArray<MentionSegment> | null): void {
		if (!content) {
			this.deleteDraft(channelId);
			return;
		}
		const normalizedSegments = normalizeDraftSegments(content, segments);
		const currentSegments = this.draftSegments[channelId] ?? EMPTY_SEGMENTS;
		if (content === this.drafts[channelId] && segmentsEqual(normalizedSegments, currentSegments)) {
			return;
		}
		this.drafts[channelId] = content;
		if (normalizedSegments.length > 0) {
			this.draftSegments[channelId] = normalizedSegments;
		} else {
			delete this.draftSegments[channelId];
		}
	}

	deleteDraft(channelId: string): void {
		if (!(channelId in this.drafts) && !(channelId in this.draftSegments)) {
			return;
		}
		delete this.drafts[channelId];
		delete this.draftSegments[channelId];
		TextareaSelection.clearChannelSelection(channelId);
	}

	deleteChannelDraft(channelId: string): void {
		this.deleteDraft(channelId);
	}

	getDraft(channelId: string): string {
		return this.drafts[channelId] ?? '';
	}

	getDraftSegments(channelId: string): ReadonlyArray<MentionSegment> {
		return this.draftSegments[channelId] ?? EMPTY_SEGMENTS;
	}

	cleanupEmptyDrafts(): void {
		for (const channelId of Object.keys(this.drafts)) {
			const content = this.drafts[channelId];
			if (!content || content.trim().length === 0) {
				delete this.drafts[channelId];
				delete this.draftSegments[channelId];
			}
		}
		for (const channelId of Object.keys(this.draftSegments)) {
			if (!(channelId in this.drafts)) {
				delete this.draftSegments[channelId];
			}
		}
	}

	getAllDrafts(): ReadonlyArray<[string, string]> {
		return Object.entries(this.drafts);
	}

	hasDraft(channelId: string): boolean {
		return channelId in this.drafts;
	}

	getDraftCount(): number {
		return Object.keys(this.drafts).length;
	}
}

export default new Drafts();
