// SPDX-License-Identifier: AGPL-3.0-or-later

import {FavoriteMeme, type FavoriteMemeWire} from '@app/features/expressions/models/FavoriteMeme';
import {makeAutoObservable} from 'mobx';

class FavoriteMemes {
	memes: ReadonlyArray<FavoriteMeme> = [];
	fetched: boolean = false;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	loadFavoriteMemes(favoriteMemes: ReadonlyArray<FavoriteMemeWire>): void {
		this.memes = Object.freeze((favoriteMemes || []).map((meme) => new FavoriteMeme(meme)));
		this.fetched = true;
	}

	reset(): void {
		this.memes = [];
		this.fetched = false;
	}

	createMeme(meme: FavoriteMemeWire): void {
		this.memes = Object.freeze([new FavoriteMeme(meme), ...this.memes]);
	}

	updateMeme(meme: FavoriteMemeWire): void {
		const index = this.memes.findIndex((m) => m.id === meme.id);
		if (index === -1) return;
		this.memes = Object.freeze([...this.memes.slice(0, index), new FavoriteMeme(meme), ...this.memes.slice(index + 1)]);
	}

	deleteMeme(memeId: string): void {
		this.memes = Object.freeze(this.memes.filter((meme) => meme.id !== memeId));
	}

	getAllMemes(): ReadonlyArray<FavoriteMeme> {
		return this.memes;
	}
}

export default new FavoriteMemes();
