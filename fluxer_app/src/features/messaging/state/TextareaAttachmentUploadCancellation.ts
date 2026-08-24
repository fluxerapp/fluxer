// SPDX-License-Identifier: AGPL-3.0-or-later

export interface CancellableTextareaAttachmentUpload {
	channelId: string;
	requestAbortController: AbortController;
	abortController: AbortController;
}

export interface TextareaAttachmentCancellationPlan<TUpload> {
	cancelled: Array<{attachmentId: number; upload: TUpload}>;
	requestControllersToAbort: Array<AbortController>;
	channelIds: Array<string>;
}

export function planTextareaAttachmentCancellation<TUpload extends CancellableTextareaAttachmentUpload>(
	uploads: ReadonlyMap<number, TUpload>,
	attachmentIds: ReadonlyArray<number>,
): TextareaAttachmentCancellationPlan<TUpload> {
	const cancelled: Array<{attachmentId: number; upload: TUpload}> = [];
	const cancelledIds = new Set<number>();
	const channelIds = new Set<string>();
	const releasedRequestControllers = new Set<AbortController>();
	for (const attachmentId of attachmentIds) {
		if (cancelledIds.has(attachmentId)) continue;
		const upload = uploads.get(attachmentId);
		if (!upload) continue;
		cancelledIds.add(attachmentId);
		cancelled.push({attachmentId, upload});
		channelIds.add(upload.channelId);
		releasedRequestControllers.add(upload.requestAbortController);
	}
	for (const [attachmentId, upload] of uploads) {
		if (cancelledIds.has(attachmentId)) continue;
		releasedRequestControllers.delete(upload.requestAbortController);
	}
	return {
		cancelled,
		requestControllersToAbort: Array.from(releasedRequestControllers),
		channelIds: Array.from(channelIds),
	};
}
