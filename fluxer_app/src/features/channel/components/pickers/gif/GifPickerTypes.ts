// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Gif} from '@app/features/expressions/commands/GifCommands';

export type View = 'default' | 'trending' | 'favorites';

export interface FavoriteGifLookup {
	url: string;
}

export type FavoriteAwareGif = Gif & {
	favoriteGifLookup?: FavoriteGifLookup;
	contentType?: string;
};
export type GifPickerGridItemData =
	| {
			type: 'category';
			key: string;
			id: string;
			title: string;
			categoryKind: 'favorites' | 'trending' | 'category';
			previewUrl: string;
			previewProxySrc: string;
			previewContentType?: string;
			width: number;
			height: number;
	  }
	| {
			type: 'gif';
			key: string;
			gif: FavoriteAwareGif;
	  }
	| {
			type: 'skeleton';
			key: string;
			width: number;
			height: number;
	  };
