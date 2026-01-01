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

import EventEmitter from 'eventemitter3';

export type ComponentActionType =
	| 'CAMERA_DEVICE_REFRESH'
	| 'CHANNEL_DETAILS_OPEN'
	| 'CHANNEL_MEMBER_LIST_TOGGLE'
	| 'CHANNEL_NOTIFICATION_SETTINGS_OPEN'
	| 'CHANNEL_PINS_OPEN'
	| 'EMOJI_PICKER_OPEN'
	| 'EMOJI_PICKER_RERENDER'
	| 'EMOJI_SELECT'
	| 'ESCAPE_PRESSED'
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
	| 'SAVED_MESSAGES_OPEN'
	| 'SCROLLTO_PRESENT'
	| 'SCROLL_PAGE_DOWN'
	| 'SCROLL_PAGE_UP'
	| 'STICKER_PICKER_RERENDER'
	| 'STICKER_SELECT'
	| 'TEXTAREA_AUTOCOMPLETE_CHANGED'
	| 'TEXTAREA_UPLOAD_FILE'
	| 'USER_SETTINGS_TAB_SELECT';

type ComponentDispatchEvents = {
	[K in ComponentActionType]: (...args: Array<unknown>) => void;
};

class Dispatch extends EventEmitter<ComponentDispatchEvents> {
	private _savedDispatches: Partial<Record<ComponentActionType, Array<unknown>>> = {};

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
			console.warn('ComponentDispatch.subscribe: Attempting to add a duplicate listener', type);
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
