// SPDX-License-Identifier: AGPL-3.0-or-later
import {filterMemesByContentType} from '@app/features/channel/components/pickers/memes/MediaFormat';
import type {ContentType} from '@app/features/channel/components/pickers/memes/MemesPickerHeader';
import MemesPicker from '@app/features/emoji/state/MemesPicker';
import type {FavoriteMeme} from '@app/features/expressions/models/FavoriteMeme';
import {matchSorter} from 'match-sorter';
import {useMemo} from 'react';
export function useFilteredMemes(
	favoriteMemes: ReadonlyArray<FavoriteMeme>,
	selectedFilter: ContentType,
	searchTerm: string,
) {
	return useMemo(() => {
		let memes = filterMemesByContentType(favoriteMemes, selectedFilter);
		if (searchTerm) {
			const sortedByMatch = matchSorter(memes, searchTerm, {
				keys: ['name', 'altText', 'filename', 'tags'],
				threshold: matchSorter.rankings.CONTAINS,
			});
			const searchIndex = new Map(sortedByMatch.map((meme, index) => [meme.id, index]));
			memes = [...sortedByMatch].sort((a, b) => {
				const frecencyDiff = MemesPicker.getFrecencyScoreForMeme(b) - MemesPicker.getFrecencyScoreForMeme(a);
				if (frecencyDiff !== 0) return frecencyDiff;
				return (searchIndex.get(a.id) ?? 0) - (searchIndex.get(b.id) ?? 0);
			});
		}
		return memes;
	}, [favoriteMemes, selectedFilter, searchTerm]);
}
