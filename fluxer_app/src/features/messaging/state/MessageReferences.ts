// SPDX-License-Identifier: AGPL-3.0-or-later

import {Message as MessageRecord} from '@app/features/messaging/models/MessagingMessage';
import Messages from '@app/features/messaging/state/MessagingMessages';
import {MessageReferenceTypes} from '@fluxer/constants/src/ChannelConstants';
import type {ValueOf} from '@fluxer/constants/src/ValueOf';
import type {Message as WireMessage} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {makeAutoObservable} from 'mobx';

export const MessageReferenceState = {
	LOADED: 'LOADED',
	NOT_LOADED: 'NOT_LOADED',
	DELETED: 'DELETED',
} as const;

export type MessageReferenceState = ValueOf<typeof MessageReferenceState>;

export type MessageReferenceResolution =
	| {readonly state: typeof MessageReferenceState.LOADED; readonly message: MessageRecord}
	| {readonly state: typeof MessageReferenceState.NOT_LOADED}
	| {readonly state: typeof MessageReferenceState.DELETED};

const NOT_LOADED_RESOLUTION: MessageReferenceResolution = Object.freeze({
	state: MessageReferenceState.NOT_LOADED,
});

const DELETED_RESOLUTION: MessageReferenceResolution = Object.freeze({
	state: MessageReferenceState.DELETED,
});

type MessageInput = WireMessage | MessageRecord;

const toWireMessage = (message: MessageInput): WireMessage =>
	message instanceof MessageRecord ? message.toJSON() : message;

class MessageReferences {
	deletedMessageIds = new Set<string>();
	cachedMessages = new Map<string, MessageRecord>();
	private referenceVersions = new Map<string, number>();
	private referenceCount = new Map<string, Set<string>>();
	private referencingMessages = new Map<
		string,
		{
			channelId: string;
			messageId: string;
		}
	>();

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	private getKey(channelId: string, messageId: string): string {
		return `${channelId}:${messageId}`;
	}

	private bumpReferenceVersion(refChannelId: string, refMessageId: string): void {
		const key = this.getKey(refChannelId, refMessageId);
		this.referenceVersions.set(key, (this.referenceVersions.get(key) ?? 0) + 1);
	}

	private readReferenceVersion(refChannelId: string, refMessageId: string): number {
		return this.referenceVersions.get(this.getKey(refChannelId, refMessageId)) ?? 0;
	}

	private setCachedMessage(refChannelId: string, refMessageId: string, message: MessageInput): boolean {
		const key = this.getKey(refChannelId, refMessageId);
		const nextMessage = new MessageRecord(toWireMessage(message), {missingReactions: 'preserve'});
		const currentMessage = this.cachedMessages.get(key);
		if (this.deletedMessageIds.has(key)) {
			this.deletedMessageIds.delete(key);
		} else if (currentMessage?.equals(nextMessage)) {
			return false;
		}
		this.cachedMessages.set(key, nextMessage);
		this.bumpReferenceVersion(refChannelId, refMessageId);
		return true;
	}

	private markReferenceDeleted(refChannelId: string, refMessageId: string): boolean {
		const key = this.getKey(refChannelId, refMessageId);
		if (this.deletedMessageIds.has(key) && !this.cachedMessages.has(key)) {
			return false;
		}
		this.deletedMessageIds.add(key);
		this.cachedMessages.delete(key);
		this.bumpReferenceVersion(refChannelId, refMessageId);
		return true;
	}

	private updateCachedMessage(refChannelId: string, refMessageId: string, updates: Partial<WireMessage>): boolean {
		const key = this.getKey(refChannelId, refMessageId);
		const currentMessage = this.cachedMessages.get(key);
		if (!currentMessage) {
			return false;
		}
		const nextMessage = currentMessage.withUpdates(updates);
		if (currentMessage.equals(nextMessage)) {
			return false;
		}
		this.cachedMessages.set(key, nextMessage);
		return true;
	}

	private handleReferencedMessageUpdate(message: WireMessage): void {
		const key = this.getKey(message.channel_id, message.id);
		const isTrackedReference = this.referenceCount.has(key) || this.cachedMessages.has(key);
		if (!isTrackedReference) {
			return;
		}
		this.updateCachedMessage(message.channel_id, message.id, message);
		this.bumpReferenceVersion(message.channel_id, message.id);
	}

	private addReference(refChannelId: string, refMessageId: string, referencingMessageId: string): void {
		const key = this.getKey(refChannelId, refMessageId);
		let refs = this.referenceCount.get(key);
		if (!refs) {
			refs = new Set<string>();
			this.referenceCount.set(key, refs);
		}
		refs.add(referencingMessageId);
		this.referencingMessages.set(referencingMessageId, {channelId: refChannelId, messageId: refMessageId});
	}

	private removeReference(refChannelId: string, refMessageId: string, referencingMessageId: string): void {
		const key = this.getKey(refChannelId, refMessageId);
		const refs = this.referenceCount.get(key);
		if (refs) {
			refs.delete(referencingMessageId);
			if (refs.size === 0) {
				this.referenceCount.delete(key);
				this.cachedMessages.delete(key);
				this.referenceVersions.delete(key);
			}
		}
		this.referencingMessages.delete(referencingMessageId);
	}

	private resolveReferenceTarget(message: WireMessage, fallbackChannelId: string): boolean {
		const reference = message.message_reference;
		if (!reference || reference.type !== MessageReferenceTypes.DEFAULT) {
			return false;
		}
		const refChannelId = reference.channel_id ?? fallbackChannelId;
		const refMessageId = reference.message_id;
		this.addReference(refChannelId, refMessageId, message.id);
		if (!('referenced_message' in message)) {
			return false;
		}
		const referenced = message.referenced_message;
		if (referenced == null) {
			return this.markReferenceDeleted(refChannelId, refMessageId);
		}
		return this.setCachedMessage(refChannelId, refMessageId, referenced);
	}

	handleMessageCreate(message: WireMessage, _optimistic: boolean): void {
		this.resolveReferenceTarget(message, message.channel_id);
	}

	handleMessageDelete(channelId: string, messageId: string): void {
		const key = this.getKey(channelId, messageId);
		this.deletedMessageIds.add(key);
		this.cachedMessages.delete(key);
		this.referenceVersions.delete(key);
		this.referenceCount.delete(key);
		const referencedBy = this.referencingMessages.get(messageId);
		if (referencedBy) {
			this.removeReference(referencedBy.channelId, referencedBy.messageId, messageId);
		}
	}

	handleMessageDeleteBulk(channelId: string, messageIds: Array<string>): void {
		for (const messageId of messageIds) {
			this.handleMessageDelete(channelId, messageId);
		}
	}

	handleMessagesFetchSuccess(channelId: string, messages: Array<WireMessage>): void {
		for (const message of messages) {
			this.resolveReferenceTarget(message, channelId);
		}
	}

	handleChannelDelete(channelId: string): void {
		this.cleanupChannelMessages(channelId);
	}

	handleGatewayReady(): void {
		this.deletedMessageIds.clear();
		this.cachedMessages.clear();
		this.referenceVersions.clear();
		this.referenceCount.clear();
		this.referencingMessages.clear();
	}

	handleMessageUpdate(message: WireMessage): void {
		this.handleReferencedMessageUpdate(message);
		if (!('message_reference' in message) && !('referenced_message' in message)) {
			return;
		}
		const reference = message.message_reference;
		const isReferenceBearing = reference != null && reference.type === MessageReferenceTypes.DEFAULT;
		const previousRef = this.referencingMessages.get(message.id);
		const newRefChannelId = reference?.channel_id ?? message.channel_id;
		const newRefMessageId = isReferenceBearing ? reference.message_id : undefined;
		if (previousRef) {
			const previousKey = this.getKey(previousRef.channelId, previousRef.messageId);
			const newKey = newRefMessageId ? this.getKey(newRefChannelId, newRefMessageId) : null;
			if (previousKey !== newKey) {
				this.removeReference(previousRef.channelId, previousRef.messageId, message.id);
			}
		}
		if (newRefMessageId) {
			this.resolveReferenceTarget(message, message.channel_id);
		}
	}

	private cleanupChannelMessages(channelId: string): void {
		const channelPrefix = `${channelId}:`;
		for (const key of Array.from(this.deletedMessageIds)) {
			if (key.startsWith(channelPrefix)) {
				this.deletedMessageIds.delete(key);
			}
		}
		for (const key of Array.from(this.cachedMessages.keys())) {
			if (key.startsWith(channelPrefix)) {
				this.cachedMessages.delete(key);
			}
		}
		for (const key of Array.from(this.referenceVersions.keys())) {
			if (key.startsWith(channelPrefix)) {
				this.referenceVersions.delete(key);
			}
		}
		for (const key of Array.from(this.referenceCount.keys())) {
			if (key.startsWith(channelPrefix)) {
				this.referenceCount.delete(key);
			}
		}
		for (const [messageId, ref] of Array.from(this.referencingMessages.entries())) {
			if (ref.channelId === channelId) {
				this.referencingMessages.delete(messageId);
			}
		}
	}

	getMessageReference(channelId: string, messageId: string): MessageReferenceResolution {
		const key = this.getKey(channelId, messageId);
		this.readReferenceVersion(channelId, messageId);
		if (this.deletedMessageIds.has(key)) {
			return DELETED_RESOLUTION;
		}
		const cachedMessage = this.cachedMessages.get(key);
		if (cachedMessage) {
			return {state: MessageReferenceState.LOADED, message: cachedMessage};
		}
		const message = Messages.getMessage(channelId, messageId);
		if (message) {
			return {state: MessageReferenceState.LOADED, message};
		}
		return NOT_LOADED_RESOLUTION;
	}
}

export default new MessageReferences();
