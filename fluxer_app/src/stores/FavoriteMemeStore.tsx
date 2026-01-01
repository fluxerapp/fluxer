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

import {makeAutoObservable} from 'mobx';
import {type FavoriteMeme, FavoriteMemeRecord} from '~/records/FavoriteMemeRecord';

class FavoriteMemeStore {
	memes: ReadonlyArray<FavoriteMemeRecord> = [];
	fetched: boolean = false;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	loadFavoriteMemes(favoriteMemes: ReadonlyArray<FavoriteMeme>): void {
		this.memes = Object.freeze((favoriteMemes || []).map((meme) => new FavoriteMemeRecord(meme)));
		this.fetched = true;
	}

	reset(): void {
		this.memes = [];
		this.fetched = false;
	}

	createMeme(meme: FavoriteMeme): void {
		this.memes = Object.freeze([new FavoriteMemeRecord(meme), ...this.memes]);
	}

	updateMeme(meme: FavoriteMeme): void {
		const index = this.memes.findIndex((m) => m.id === meme.id);
		if (index === -1) return;

		this.memes = Object.freeze([
			...this.memes.slice(0, index),
			new FavoriteMemeRecord(meme),
			...this.memes.slice(index + 1),
		]);
	}

	deleteMeme(memeId: string): void {
		this.memes = Object.freeze(this.memes.filter((meme) => meme.id !== memeId));
	}

	getAllMemes(): ReadonlyArray<FavoriteMemeRecord> {
		return this.memes;
	}

	getMeme(memeId: string): FavoriteMemeRecord | undefined {
		return this.memes.find((meme) => meme.id === memeId);
	}
}

export default new FavoriteMemeStore();
