// SPDX-License-Identifier: AGPL-3.0-or-later

export type InputEvent =
	| {
			type: 'keydown' | 'keyup';
			keycode: number;
			keyName: string;
			ctrlKey: boolean;
			altKey: boolean;
			shiftKey: boolean;
			metaKey: boolean;
	  }
	| {
			type: 'mousedown' | 'mouseup';
			button: number;
			ctrlKey: boolean;
			altKey: boolean;
			shiftKey: boolean;
			metaKey: boolean;
			x?: number;
			y?: number;
	  };

export declare class InputHook {
	constructor(callback: (event: InputEvent) => void);

	start(): void;

	stop(): void;
}

export declare function isAvailable(): boolean;

export declare function hasAccessibilityPermission(): boolean;

export declare const loadError: Error | null;
