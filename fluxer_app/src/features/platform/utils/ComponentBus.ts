// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import EventEmitter from 'eventemitter3';

export type ComponentActionType =
	| 'CAMERA_DEVICE_REFRESH'
	| 'CHANNEL_DETAILS_OPEN'
	| 'CHANNEL_MEMBER_LIST_TOGGLE'
	| 'CHANNEL_NOTIFICATION_SETTINGS_OPEN'
	| 'CHANNEL_PINS_OPEN'
	| 'COMPACT_VOICE_CALL_EXPANSION_TOGGLE'
	| 'EMOJI_PICKER_OPEN'
	| 'EMOJI_PICKER_RERENDER'
	| 'EMOJI_SELECT'
	| 'ESCAPE_PRESSED'
	| 'EDITING_EXPRESSION_PICKER_TAB_TOGGLE'
	| 'EXPRESSION_PICKER_TAB_TOGGLE'
	| 'FAVORITE_MEME_SELECT'
	| 'FOCUS_BOTTOMMOST_MESSAGE'
	| 'FOCUS_TEXTAREA'
	| 'FORCE_JUMP_TO_PRESENT'
	| 'GIF_SELECT'
	| 'INBOX_OPEN'
	| 'INSERT_MENTION'
	| 'LAYOUT_RESIZED'
	| 'MEMES_PICKER_RERENDER'
	| 'MESSAGE_SEARCH_OPEN'
	| 'MESSAGE_SENT'
	| 'OPEN_MEMES_TAB'
	| 'POPOUT_CLOSE'
	| 'SCROLLTO_PRESENT'
	| 'SCROLL_PAGE_DOWN'
	| 'SCROLL_PAGE_UP'
	| 'SOUNDBOARD_TOGGLE'
	| 'STICKER_PICKER_RERENDER'
	| 'STICKER_SELECT'
	| 'TEXTAREA_SEND_VOICE_MESSAGE'
	| 'TEXTAREA_DISMISS_AFFORDANCE'
	| 'TEXTAREA_UPLOAD_FILE'
	| 'USER_SETTINGS_TAB_SELECT';
type ComponentDispatchEvents = {
	[K in ComponentActionType]: (...args: Array<unknown>) => unknown;
};

class Dispatch extends EventEmitter<ComponentDispatchEvents> {
	private _savedDispatches: Partial<Record<ComponentActionType, Array<unknown>>> = {};
	private logger = new Logger('ComponentDispatch');

	safeDispatch(type: ComponentActionType, args?: unknown) {
		if (!this.hasSubscribers(type)) {
			if (!this._savedDispatches[type]) {
				this._savedDispatches[type] = [];
			}
			this._savedDispatches[type].push(args);
			return;
		}
		this.dispatch(type, args);
	}

	dispatch(type: ComponentActionType, args?: unknown) {
		this.emit(type, args);
	}

	dispatchToLastSubscribed(type: ComponentActionType, args?: unknown) {
		const listeners = this.listeners(type);
		if (listeners.length > 0) {
			listeners[listeners.length - 1](args);
		}
	}

	dispatchToFirstResult(type: ComponentActionType, args: unknown, predicate: (result: unknown) => boolean): unknown {
		for (const listener of this.listeners(type)) {
			const result = listener(args);
			if (predicate(result)) return result;
		}
		return undefined;
	}

	dispatchToFirst(types: Array<ComponentActionType>, args?: unknown) {
		for (const type of types) {
			if (this.hasSubscribers(type)) {
				this.dispatch(type, args);
				break;
			}
		}
	}

	hasSubscribers(type: ComponentActionType) {
		return this.listenerCount(type) > 0;
	}

	private _checkSavedDispatches(type: ComponentActionType) {
		if (this._savedDispatches[type]) {
			for (const args of this._savedDispatches[type]) {
				this.dispatch(type, args);
			}
			delete this._savedDispatches[type];
		}
	}

	subscribe(type: ComponentActionType, callback: (...args: Array<unknown>) => void): () => void {
		if (this.listeners(type).includes(callback)) {
			this.logger.warn('ComponentDispatch.subscribe: Attempting to add a duplicate listener', type);
			return () => {
				this.unsubscribe(type, callback);
			};
		}
		this.on(type, callback);
		this._checkSavedDispatches(type);
		return () => {
			this.unsubscribe(type, callback);
		};
	}

	subscribeOnce(type: ComponentActionType, callback: (...args: Array<unknown>) => void): () => void {
		this.once(type, callback);
		this._checkSavedDispatches(type);
		return () => {
			this.unsubscribe(type, callback);
		};
	}

	unsubscribe(type: ComponentActionType, callback: (...args: Array<unknown>) => void) {
		this.removeListener(type, callback);
	}

	reset() {
		this.removeAllListeners();
	}
}

export const ComponentDispatch = new Dispatch();
