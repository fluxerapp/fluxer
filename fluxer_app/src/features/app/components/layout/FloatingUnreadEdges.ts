// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ScrollIndicatorSeverity} from '@app/features/app/components/layout/ScrollIndicatorStateMachine';

export interface FloatingUnreadTarget {
	id: string;
	severity: ScrollIndicatorSeverity;
}

export interface MeasuredFloatingUnreadTarget {
	id: string;
	severity: ScrollIndicatorSeverity;
	top: number;
	bottom: number;
}

export interface FloatingUnreadEdges {
	top: MeasuredFloatingUnreadTarget | null;
	bottom: MeasuredFloatingUnreadTarget | null;
}

export interface FloatingUnreadTargetIndex {
	byBottom: ReadonlyArray<MeasuredFloatingUnreadTarget>;
	topCandidates: ReadonlyArray<MeasuredFloatingUnreadTarget>;
	byTop: ReadonlyArray<MeasuredFloatingUnreadTarget>;
	bottomCandidates: ReadonlyArray<MeasuredFloatingUnreadTarget>;
}

export interface FloatingUnreadTargetBounds {
	readonly bottom: number;
	readonly top: number;
}

export interface FloatingUnreadViewport {
	scrollTop: number;
	viewportHeight: number;
	targetIndex: FloatingUnreadTargetIndex;
}

interface CreateFloatingUnreadTargetIndexFromMeasurementsRequest {
	readonly targets: ReadonlyArray<MeasuredFloatingUnreadTarget>;
}

interface CreateFloatingUnreadTargetIndexFromBoundsRequest {
	readonly boundsByTargetId: ReadonlyMap<string, FloatingUnreadTargetBounds>;
	readonly targets: ReadonlyArray<FloatingUnreadTarget>;
}

interface CompareFloatingUnreadEdgesQuery {
	readonly left: FloatingUnreadEdges;
	readonly right: FloatingUnreadEdges;
}

interface PreserveStableFloatingUnreadEdgesQuery {
	readonly current: FloatingUnreadEdges;
	readonly next: FloatingUnreadEdges;
}

const VISIBILITY_EPSILON = 0.5;
const EMPTY_MEASURED_TARGETS: ReadonlyArray<MeasuredFloatingUnreadTarget> = Object.freeze([]);

function compareTargetIdentifiers(left: string, right: string): number {
	if (left === right) return 0;
	if (left < right) return -1;
	return 1;
}

class FloatingUnreadEdgeLayoutOwner {
	public readonly EMPTY_EDGES: FloatingUnreadEdges = Object.freeze({top: null, bottom: null});
	public readonly EMPTY_TARGET_INDEX: FloatingUnreadTargetIndex = Object.freeze({
		byBottom: EMPTY_MEASURED_TARGETS,
		topCandidates: EMPTY_MEASURED_TARGETS,
		byTop: EMPTY_MEASURED_TARGETS,
		bottomCandidates: EMPTY_MEASURED_TARGETS,
	});

	public createTargetIndexFromMeasurements({
		targets,
	}: CreateFloatingUnreadTargetIndexFromMeasurementsRequest): FloatingUnreadTargetIndex {
		if (targets.length === 0) return this.EMPTY_TARGET_INDEX;

		const byBottom = targets.slice().sort(this.compareByBottom);
		const topCandidates = new Array<MeasuredFloatingUnreadTarget>(byBottom.length);
		let topCandidate: MeasuredFloatingUnreadTarget | null = null;
		for (let index = 0; index < byBottom.length; index++) {
			topCandidate = this.chooseTopCandidate(topCandidate, byBottom[index]);
			topCandidates[index] = topCandidate;
		}

		const byTop = targets.slice().sort(this.compareByTop);
		const bottomCandidates = new Array<MeasuredFloatingUnreadTarget>(byTop.length);
		let bottomCandidate: MeasuredFloatingUnreadTarget | null = null;
		for (let index = byTop.length - 1; index >= 0; index--) {
			bottomCandidate = this.chooseBottomCandidate(byTop[index], bottomCandidate);
			bottomCandidates[index] = bottomCandidate;
		}

		return {byBottom, topCandidates, byTop, bottomCandidates};
	}

	public createTargetIndexFromBounds({
		targets,
		boundsByTargetId,
	}: CreateFloatingUnreadTargetIndexFromBoundsRequest): FloatingUnreadTargetIndex {
		if (targets.length === 0) return this.EMPTY_TARGET_INDEX;
		const measuredTargets: Array<MeasuredFloatingUnreadTarget> = [];
		for (const target of targets) {
			const bounds = boundsByTargetId.get(target.id);
			if (bounds == null) continue;
			measuredTargets.push({...target, ...bounds});
		}
		return this.createTargetIndexFromMeasurements({targets: measuredTargets});
	}

	public selectEdges({scrollTop, viewportHeight, targetIndex}: FloatingUnreadViewport): FloatingUnreadEdges {
		const viewportTop = Math.max(0, scrollTop);
		const viewportBottom = viewportTop + Math.max(0, viewportHeight);
		const topIndex = this.findLastTargetEndingBefore(targetIndex.byBottom, viewportTop + VISIBILITY_EPSILON);
		const bottomIndex = this.findFirstTargetStartingAfter(targetIndex.byTop, viewportBottom - VISIBILITY_EPSILON);
		let top: MeasuredFloatingUnreadTarget | null;
		if (topIndex >= 0) {
			top = targetIndex.topCandidates[topIndex];
		} else {
			top = null;
		}
		let bottom: MeasuredFloatingUnreadTarget | null;
		if (bottomIndex < targetIndex.byTop.length) {
			bottom = targetIndex.bottomCandidates[bottomIndex];
		} else {
			bottom = null;
		}
		return {top, bottom};
	}

	public areEdgesEqual({left, right}: CompareFloatingUnreadEdgesQuery): boolean {
		return this.areEdgesEqualAtPosition(left.top, right.top) && this.areEdgesEqualAtPosition(left.bottom, right.bottom);
	}

	public preserveStableEdges({current, next}: PreserveStableFloatingUnreadEdgesQuery): FloatingUnreadEdges {
		if (this.areEdgesEqual({left: current, right: next})) {
			return current;
		}
		return next;
	}

	private chooseTopCandidate(
		current: MeasuredFloatingUnreadTarget | null,
		next: MeasuredFloatingUnreadTarget,
	): MeasuredFloatingUnreadTarget {
		if (current == null || (current.severity !== 'mention' && next.severity === 'mention')) {
			return next;
		}
		return current;
	}

	private chooseBottomCandidate(
		current: MeasuredFloatingUnreadTarget,
		next: MeasuredFloatingUnreadTarget | null,
	): MeasuredFloatingUnreadTarget {
		if (next == null || (next.severity !== 'mention' && current.severity === 'mention')) {
			return current;
		}
		return next;
	}

	private compareByBottom(left: MeasuredFloatingUnreadTarget, right: MeasuredFloatingUnreadTarget): number {
		const bottomDifference = left.bottom - right.bottom;
		if (bottomDifference !== 0) return bottomDifference;
		const topDifference = left.top - right.top;
		if (topDifference !== 0) return topDifference;
		return compareTargetIdentifiers(left.id, right.id);
	}

	private compareByTop(left: MeasuredFloatingUnreadTarget, right: MeasuredFloatingUnreadTarget): number {
		const topDifference = left.top - right.top;
		if (topDifference !== 0) return topDifference;
		const bottomDifference = left.bottom - right.bottom;
		if (bottomDifference !== 0) return bottomDifference;
		return compareTargetIdentifiers(left.id, right.id);
	}

	private findLastTargetEndingBefore(
		targets: ReadonlyArray<MeasuredFloatingUnreadTarget>,
		viewportTop: number,
	): number {
		let low = 0;
		let high = targets.length;
		while (low < high) {
			const middle = low + Math.floor((high - low) / 2);
			if (targets[middle].bottom <= viewportTop) {
				low = middle + 1;
			} else {
				high = middle;
			}
		}
		return low - 1;
	}

	private findFirstTargetStartingAfter(
		targets: ReadonlyArray<MeasuredFloatingUnreadTarget>,
		viewportBottom: number,
	): number {
		let low = 0;
		let high = targets.length;
		while (low < high) {
			const middle = low + Math.floor((high - low) / 2);
			if (targets[middle].top < viewportBottom) {
				low = middle + 1;
			} else {
				high = middle;
			}
		}
		return low;
	}

	private areEdgesEqualAtPosition(
		left: MeasuredFloatingUnreadTarget | null,
		right: MeasuredFloatingUnreadTarget | null,
	): boolean {
		if (left === right) return true;
		if (left == null || right == null) return false;
		return left.id === right.id && left.severity === right.severity;
	}
}

export const FloatingUnreadEdgeLayout = Object.freeze(new FloatingUnreadEdgeLayoutOwner());
