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

import {BehaviorSubject, firstValueFrom, interval, type Observable, type Subscription} from 'rxjs';
import {filter, map, timeout as rxTimeout, take} from 'rxjs/operators';
import {MessageAttachmentFlags} from '~/Constants';
import {Logger} from '~/lib/Logger';
import type {AllowedMentions} from '~/records/MessageRecord';

const logger = new Logger('CloudUpload');
const hasDOM = true;

export type UploadStatus = 'pending' | 'uploading' | 'failed' | 'sending';

export type UploadStage = 'idle' | 'queued' | 'uploading' | 'processing' | 'completed' | 'failed' | 'canceled';

export interface CloudAttachment {
	id: number;
	channelId: string;
	file: File;
	filename: string;
	description?: string;
	flags: number;
	previewURL: string | null;
	thumbnailURL: string | null;
	width: number;
	height: number;
	status: UploadStatus;
	uploadProgress?: number;
	spoiler?: boolean;
}

export interface MessageUpload {
	nonce: string;
	channelId: string;
	attachments: Array<CloudAttachment>;
	content?: string;
	messageReference?: {message_id: string};
	allowedMentions?: AllowedMentions;
	flags?: number;

	sendingProgress?: number;

	stage: UploadStage;
}

type Listener = () => void;
type MessageUploadListener = (upload: MessageUpload) => void;

class CloudUploadManager {
	private static readonly MESSAGE_UPLOAD_TTL_MS = 5 * 60 * 1000;
	private static readonly CLEANUP_INTERVAL_MS = 60 * 1000;

	private nextAttachmentId = 1;

	private readonly textareaAttachmentSubjects = new Map<string, BehaviorSubject<Array<CloudAttachment>>>();

	private readonly messageUploadSubjects = new Map<string, BehaviorSubject<MessageUpload | null>>();

	private readonly messageUploadTimestamps: Map<string, number> = new Map();

	private readonly activeUploadControllers: Map<number, AbortController> = new Map();

	private readonly attachmentIndex: Map<number, CloudAttachment> = new Map();

	private readonly textareaListeners: Map<string, Set<Listener>> = new Map();
	private readonly messageUploadListeners: Map<string, Set<MessageUploadListener>> = new Map();

	private cleanupSubscription: Subscription | null = null;

	constructor() {
		this.cleanupSubscription = interval(CloudUploadManager.CLEANUP_INTERVAL_MS).subscribe(() => {
			this.cleanupStaleUploads();
		});
	}

	destroy(): void {
		if (this.cleanupSubscription) {
			this.cleanupSubscription.unsubscribe();
			this.cleanupSubscription = null;
		}

		for (const subject of this.textareaAttachmentSubjects.values()) {
			for (const att of subject.value) {
				if (att.previewURL) URL.revokeObjectURL(att.previewURL);
				if (att.thumbnailURL) URL.revokeObjectURL(att.thumbnailURL);
			}
		}

		for (const subject of this.messageUploadSubjects.values()) {
			const upload = subject.value;
			if (!upload) continue;
			for (const att of upload.attachments) {
				if (att.previewURL) URL.revokeObjectURL(att.previewURL);
				if (att.thumbnailURL) URL.revokeObjectURL(att.thumbnailURL);
			}
		}

		this.textareaAttachmentSubjects.clear();
		this.messageUploadSubjects.clear();
		this.attachmentIndex.clear();
		this.messageUploadTimestamps.clear();
		this.activeUploadControllers.clear();
		this.textareaListeners.clear();
		this.messageUploadListeners.clear();
	}

	private ensureTextareaSubject(channelId: string): BehaviorSubject<Array<CloudAttachment>> {
		let subject = this.textareaAttachmentSubjects.get(channelId);
		if (!subject) {
			subject = new BehaviorSubject<Array<CloudAttachment>>([]);
			this.textareaAttachmentSubjects.set(channelId, subject);
		}
		return subject;
	}

	private ensureMessageUploadSubject(nonce: string): BehaviorSubject<MessageUpload | null> {
		let subject = this.messageUploadSubjects.get(nonce);
		if (!subject) {
			subject = new BehaviorSubject<MessageUpload | null>(null);
			this.messageUploadSubjects.set(nonce, subject);
		}
		return subject;
	}

	private getMessageUploadInternal(nonce: string): MessageUpload | null {
		const subject = this.messageUploadSubjects.get(nonce);
		return subject?.value ?? null;
	}

	private setMessageUploadInternal(nonce: string, upload: MessageUpload | null): void {
		const subject = this.ensureMessageUploadSubject(nonce);

		if (upload) {
			this.messageUploadTimestamps.set(nonce, Date.now());
		} else {
			this.messageUploadTimestamps.delete(nonce);
		}

		subject.next(upload);
	}

	private updateMessageUpload(nonce: string, updater: (current: MessageUpload) => MessageUpload): void {
		const current = this.getMessageUploadInternal(nonce);
		if (!current) return;

		const updated = updater(current);
		this.setMessageUploadInternal(nonce, updated);
		this.notifyMessageUploadListeners(nonce);
	}

	private cleanupStaleUploads(): void {
		const now = Date.now();
		const staleNonces: Array<string> = [];

		for (const [nonce, timestamp] of this.messageUploadTimestamps.entries()) {
			if (now - timestamp > CloudUploadManager.MESSAGE_UPLOAD_TTL_MS) {
				staleNonces.push(nonce);
			}
		}

		for (const nonce of staleNonces) {
			logger.debug(`Cleaning up stale upload for nonce ${nonce}`);
			this.removeMessageUpload(nonce);
		}
	}

	attachments$(channelId: string): Observable<ReadonlyArray<CloudAttachment>> {
		return this.ensureTextareaSubject(channelId).asObservable();
	}

	messageUpload$(nonce: string): Observable<MessageUpload | null> {
		return this.ensureMessageUploadSubject(nonce).asObservable();
	}

	getTextareaAttachments(channelId: string): Array<CloudAttachment> {
		return this.ensureTextareaSubject(channelId).value;
	}

	subscribeToTextarea(channelId: string, listener: Listener): () => void {
		if (!this.textareaListeners.has(channelId)) {
			this.textareaListeners.set(channelId, new Set());
		}
		this.textareaListeners.get(channelId)!.add(listener);

		const subject = this.ensureTextareaSubject(channelId);
		const subscription = subject.subscribe(() => listener());

		return () => {
			subscription.unsubscribe();
			const listeners = this.textareaListeners.get(channelId);
			if (listeners) {
				listeners.delete(listener);
				if (listeners.size === 0) {
					this.textareaListeners.delete(channelId);
				}
			}
		};
	}

	private notifyTextareaListeners(channelId: string): void {
		const listeners = this.textareaListeners.get(channelId);
		if (!listeners) return;
		listeners.forEach((listener) => listener());
	}

	getMessageUpload(nonce: string): MessageUpload | null {
		return this.getMessageUploadInternal(nonce);
	}

	subscribeToMessageUpload(nonce: string, listener: MessageUploadListener): () => void {
		if (!this.messageUploadListeners.has(nonce)) {
			this.messageUploadListeners.set(nonce, new Set());
		}
		this.messageUploadListeners.get(nonce)!.add(listener);

		const subject = this.ensureMessageUploadSubject(nonce);
		const subscription = subject.subscribe((upload) => {
			if (upload) listener(upload);
		});

		return () => {
			subscription.unsubscribe();
			const listeners = this.messageUploadListeners.get(nonce);
			if (listeners) {
				listeners.delete(listener);
				if (listeners.size === 0) {
					this.messageUploadListeners.delete(nonce);
				}
			}
		};
	}

	private notifyMessageUploadListeners(nonce: string): void {
		const listeners = this.messageUploadListeners.get(nonce);
		const upload = this.getMessageUploadInternal(nonce);
		if (!listeners || !upload) return;

		listeners.forEach((listener) => listener(upload));
	}

	async addFiles(channelId: string, files: Array<File>): Promise<void> {
		if (files.length === 0) return;

		const newAttachments = await this.createAttachments(channelId, files);
		const subject = this.ensureTextareaSubject(channelId);
		const existing = subject.value;

		const combined = [...existing, ...newAttachments];
		subject.next(combined);

		newAttachments.forEach((att) => this.attachmentIndex.set(att.id, att));

		this.notifyTextareaListeners(channelId);
	}

	async createAndStartUploads(channelId: string, files: Array<File>): Promise<Array<CloudAttachment>> {
		const attachments = await this.createAttachments(channelId, files);

		attachments.forEach((att) => {
			this.attachmentIndex.set(att.id, att);
		});

		return attachments;
	}

	removeAttachment(channelId: string, attachmentId: number): void {
		const subject = this.ensureTextareaSubject(channelId);
		const attachments = subject.value;

		const attachment = attachments.find((a) => a.id === attachmentId);

		if (attachment) {
			this.attachmentIndex.delete(attachmentId);

			if (attachment.previewURL) URL.revokeObjectURL(attachment.previewURL);
			if (attachment.thumbnailURL) URL.revokeObjectURL(attachment.thumbnailURL);
		}

		const next = attachments.filter((a) => a.id !== attachmentId);
		subject.next(next);

		this.notifyTextareaListeners(channelId);
	}

	updateAttachment(channelId: string, attachmentId: number, patch: Partial<CloudAttachment>): void {
		const subject = this.ensureTextareaSubject(channelId);
		const attachments = subject.value;
		const index = attachments.findIndex((a) => a.id === attachmentId);
		if (index === -1) return;

		const updated = {...attachments[index], ...patch};

		const next = [...attachments];
		next[index] = updated;
		subject.next(next);

		this.attachmentIndex.set(attachmentId, updated);

		this.notifyTextareaListeners(channelId);
	}

	clearTextarea(channelId: string, preserveBlobs = false): void {
		const subject = this.ensureTextareaSubject(channelId);
		const attachments = subject.value;

		attachments.forEach((att) => {
			this.attachmentIndex.delete(att.id);
		});

		if (!preserveBlobs) {
			attachments.forEach((att) => {
				if (att.previewURL) URL.revokeObjectURL(att.previewURL);
				if (att.thumbnailURL) URL.revokeObjectURL(att.thumbnailURL);
			});
		}

		subject.next([]);

		this.notifyTextareaListeners(channelId);
	}

	reorderAttachments(channelId: string, newOrder: Array<CloudAttachment>): void {
		this.ensureTextareaSubject(channelId).next([...newOrder]);

		newOrder.forEach((att) => this.attachmentIndex.set(att.id, att));

		this.notifyTextareaListeners(channelId);
	}

	claimAttachmentsForMessage(
		channelId: string,
		nonce: string,
		attachments?: Array<CloudAttachment>,
		metadata?: {
			content?: string;
			messageReference?: {message_id: string};
			allowedMentions?: AllowedMentions;
			flags?: number;
		},
	): Array<CloudAttachment> {
		const subject = this.ensureTextareaSubject(channelId);
		const attachmentsToUse = attachments ?? subject.value;
		if (attachmentsToUse.length === 0) return [];

		const clonedAttachments = attachmentsToUse.map((att) => ({...att}));

		const upload: MessageUpload = {
			nonce,
			channelId,
			attachments: clonedAttachments,
			stage: 'queued',
			...metadata,
		};

		this.setMessageUploadInternal(nonce, upload);

		clonedAttachments.forEach((att) => {
			this.attachmentIndex.set(att.id, att);
		});

		if (!attachments) {
			subject.next([]);
			this.notifyTextareaListeners(channelId);
		}

		this.notifyMessageUploadListeners(nonce);

		return clonedAttachments;
	}

	moveMessageUpload(oldNonce: string, newNonce: string): void {
		const upload = this.getMessageUploadInternal(oldNonce);
		if (!upload) return;

		const moved: MessageUpload = {
			...upload,
			nonce: newNonce,
		};

		this.setMessageUploadInternal(newNonce, moved);
		this.setMessageUploadInternal(oldNonce, null);

		const listeners = this.messageUploadListeners.get(oldNonce);
		if (listeners) {
			this.messageUploadListeners.delete(oldNonce);
			if (!this.messageUploadListeners.has(newNonce)) {
				this.messageUploadListeners.set(newNonce, new Set());
			}
			const target = this.messageUploadListeners.get(newNonce)!;
			for (const listener of listeners) {
				target.add(listener);
			}
		}

		this.notifyMessageUploadListeners(newNonce);
	}

	removeMessageUpload(nonce: string): void {
		const upload = this.getMessageUploadInternal(nonce);
		if (upload) {
			for (const att of upload.attachments) {
				this.attachmentIndex.delete(att.id);
				if (att.previewURL) URL.revokeObjectURL(att.previewURL);
				if (att.thumbnailURL) URL.revokeObjectURL(att.thumbnailURL);
			}
		}

		this.setMessageUploadInternal(nonce, null);
		this.messageUploadListeners.delete(nonce);
	}

	restoreAttachmentsToTextarea(nonce: string): void {
		const upload = this.getMessageUploadInternal(nonce);
		if (!upload) return;

		const subject = this.ensureTextareaSubject(upload.channelId);
		subject.next(upload.attachments);

		upload.attachments.forEach((att) => {
			this.attachmentIndex.set(att.id, att);
		});

		this.notifyTextareaListeners(upload.channelId);

		this.setMessageUploadInternal(nonce, null);
		this.messageUploadListeners.delete(nonce);
	}

	updateSendingProgress(nonce: string, progressPercent: number): void {
		const clamped = Number.isFinite(progressPercent) ? Math.min(100, Math.max(0, progressPercent)) : 0;

		const normalized = clamped / 100;

		this.updateMessageUpload(nonce, (upload) => {
			const attachments = upload.attachments;
			if (attachments.length === 0) {
				return {
					...upload,
					stage: normalized >= 1 ? 'completed' : 'uploading',
					sendingProgress: clamped,
				};
			}

			const totalSize = attachments.reduce((sum, att) => sum + (att.file?.size ?? 0), 0);
			const safeTotal = totalSize > 0 ? totalSize : attachments.length;

			let cursor = 0;

			const nextAttachments = attachments.map((att) => {
				const size = att.file?.size ?? 1;
				const ratio = size / safeTotal;

				const start = cursor;
				const end = cursor + ratio;
				cursor = end;

				let local = 0;
				if (normalized <= start) {
					local = 0;
				} else if (normalized >= end) {
					local = 1;
				} else {
					local = (normalized - start) / ratio;
				}

				const uploadProgress = Math.round(local * 100);

				let status = att.status;
				if (status === 'pending' && uploadProgress > 0) {
					status = 'uploading';
				}
				if (status === 'uploading' && uploadProgress === 100) {
					status = 'sending';
				}

				return {
					...att,
					status,
					uploadProgress,
				};
			});

			return {
				...upload,
				stage: normalized >= 1 ? 'completed' : 'uploading',
				sendingProgress: clamped,
				attachments: nextAttachments,
			};
		});
	}

	async waitForMessageUploads(nonce: string, timeoutMs = 60_000): Promise<Array<CloudAttachment>> {
		const source$ = this.messageUpload$(nonce).pipe(
			filter((upload): upload is MessageUpload => upload !== null),
			map((upload) => {
				const hasFailed = upload.attachments.some((att) => att.status === 'failed');
				if (hasFailed) {
					throw new Error('One or more attachments failed to upload');
				}

				const allDone = upload.attachments.every((att) => att.status !== 'pending' && att.status !== 'uploading');

				return allDone ? upload.attachments : null;
			}),
			filter((attachments): attachments is Array<CloudAttachment> => attachments !== null),
			take(1),
			rxTimeout({first: timeoutMs}),
		);

		return firstValueFrom(source$);
	}

	startSendingProgress(nonce: string): void {
		this.updateMessageUpload(nonce, (upload) => {
			const attachments = upload.attachments.map((att) => {
				if (att.status === 'pending') {
					return {...att, status: 'uploading' as const, uploadProgress: att.uploadProgress ?? 0};
				}
				return att;
			});

			return {
				...upload,
				stage: 'uploading' as const,
				sendingProgress: upload.sendingProgress ?? 0,
				attachments,
			};
		});
	}

	stopSendingProgress(_nonce: string): void {}

	async cancelUpload(attachmentId: number): Promise<void> {
		const attachment = this.attachmentIndex.get(attachmentId);
		if (!attachment) {
			logger.warn(`Cannot cancel upload: attachment ${attachmentId} not found`);
			return;
		}

		const controller = this.activeUploadControllers.get(attachmentId);
		if (controller) {
			controller.abort();
			this.activeUploadControllers.delete(attachmentId);
		}

		if (attachment.previewURL) URL.revokeObjectURL(attachment.previewURL);
		if (attachment.thumbnailURL) URL.revokeObjectURL(attachment.thumbnailURL);

		this.attachmentIndex.delete(attachmentId);

		for (const [nonce, subject] of this.messageUploadSubjects.entries()) {
			const upload = subject.value;
			if (!upload) continue;

			const index = upload.attachments.findIndex((a) => a.id === attachmentId);
			if (index === -1) continue;

			const nextAttachments = upload.attachments.slice();
			nextAttachments.splice(index, 1);

			const updated: MessageUpload = {
				...upload,
				attachments: nextAttachments,
			};

			this.setMessageUploadInternal(nonce, updated);
			this.notifyMessageUploadListeners(nonce);
			break;
		}

		const channelId = attachment.channelId;
		const textareaSubject = this.textareaAttachmentSubjects.get(channelId);
		if (textareaSubject) {
			const next = textareaSubject.value.filter((a) => a.id !== attachmentId);
			textareaSubject.next(next);
			this.notifyTextareaListeners(channelId);
		}

		logger.debug(`Cancelled upload for attachment ${attachmentId}`);
	}

	private async createAttachments(channelId: string, files: Array<File>): Promise<Array<CloudAttachment>> {
		return Promise.all(
			files.map(async (file) => {
				let width = 0;
				let height = 0;
				let thumbnailURL: string | null = null;
				const spoiler = file.name.startsWith('SPOILER_');

				if (hasDOM) {
					try {
						if (file.type.startsWith('image/')) {
							const dims = await this.getImageDimensions(file);
							width = dims.width;
							height = dims.height;
						} else if (file.type.startsWith('video/')) {
							const data = await this.getVideoData(file);
							width = data.width;
							height = data.height;
							thumbnailURL = data.thumbnailURL;
						}
					} catch (error) {
						logger.warn('Error getting file data:', error);
					}
				}

				const isMedia =
					file.type.startsWith('image/') || file.type.startsWith('video/') || file.type.startsWith('audio/');
				const previewURL = hasDOM && isMedia ? URL.createObjectURL(file) : null;
				const flags = spoiler ? MessageAttachmentFlags.IS_SPOILER : 0;

				const attachment: CloudAttachment = {
					id: this.nextAttachmentId++,
					channelId,
					file,
					filename: file.name,
					description: undefined,
					flags,
					previewURL,
					thumbnailURL,
					status: 'pending',
					width,
					height,
					uploadProgress: 0,
					spoiler,
				};

				return attachment;
			}),
		);
	}

	private async getImageDimensions(file: File): Promise<{width: number; height: number}> {
		if (!hasDOM) return {width: 0, height: 0};
		return new Promise((resolve, reject) => {
			const url = URL.createObjectURL(file);
			const img = new Image();

			img.onload = () => {
				const {naturalWidth: width, naturalHeight: height} = img;
				URL.revokeObjectURL(url);
				resolve({width, height});
			};

			img.onerror = (error) => {
				URL.revokeObjectURL(url);
				reject(error);
			};

			img.src = url;
		});
	}

	private async getVideoData(file: File): Promise<{width: number; height: number; thumbnailURL: string | null}> {
		if (!hasDOM) return {width: 0, height: 0, thumbnailURL: null};
		return new Promise((resolve, reject) => {
			const url = URL.createObjectURL(file);
			const video = document.createElement('video');
			const canvas = document.createElement('canvas');
			const ctx = canvas.getContext('2d');

			video.addEventListener('loadedmetadata', () => {
				const {videoWidth: width, videoHeight: height} = video;

				if (!ctx) {
					URL.revokeObjectURL(url);
					resolve({width, height, thumbnailURL: null});
					return;
				}

				video.currentTime = 0;
			});

			video.addEventListener('seeked', () => {
				const {videoWidth: width, videoHeight: height} = video;

				if (!ctx) {
					URL.revokeObjectURL(url);
					resolve({width, height, thumbnailURL: null});
					return;
				}

				canvas.width = width;
				canvas.height = height;
				ctx.drawImage(video, 0, 0, width, height);

				canvas.toBlob(
					(blob) => {
						URL.revokeObjectURL(url);
						const thumbnailURL = blob ? URL.createObjectURL(blob) : null;
						resolve({width, height, thumbnailURL});
					},
					'image/jpeg',
					0.8,
				);
			});

			video.addEventListener('error', (e) => {
				URL.revokeObjectURL(url);
				reject(e);
			});

			video.src = url;
		});
	}
}

export const CloudUpload = new CloudUploadManager();
