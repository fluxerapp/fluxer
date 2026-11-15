// SPDX-License-Identifier: AGPL-3.0-or-later

export interface SystemDictionary {
	tag: string;
	affPath: string;
	dicPath: string;
}

export declare class Hunspell {
	constructor(affPath: string, dicPath: string);

	spell(word: string): boolean;

	suggest(word: string, max?: number): Array<string>;

	add(word: string): void;

	remove(word: string): void;

	close(): void;
}

export declare function discoverSystemDictionaries(): Array<SystemDictionary>;

export declare const loadError: Error | null;
