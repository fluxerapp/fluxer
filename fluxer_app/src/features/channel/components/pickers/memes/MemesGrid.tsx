// SPDX-License-Identifier: AGPL-3.0-or-later

import {MemeGridItem} from '@app/features/channel/components/pickers/memes/MemeGridItem';
import {computeMasonryColumns} from '@app/features/channel/components/pickers/shared/ComputeColumns';
import {MasonryVirtualGrid} from '@app/features/channel/components/pickers/shared/MasonryVirtualGrid';
import MemesPicker from '@app/features/emoji/state/MemesPicker';
import type {FavoriteMeme} from '@app/features/expressions/models/FavoriteMeme';
import QuickSwitcher from '@app/features/search/state/QuickSwitcher';
import {observer} from 'mobx-react-lite';
import {useCallback, useMemo} from 'react';

export const MemesGrid = observer(
	({
		memes,
		onClose,
		viewportWidth,
		viewportHeight,
		scrollTop,
		onContentSizeChange,
	}: {
		memes: Array<FavoriteMeme>;
		onClose?: () => void;
		viewportWidth: number;
		viewportHeight: number;
		scrollTop: number;
		onContentSizeChange?: (contentSize: number) => void;
	}) => {
		const tileSpacing = 8;
		const columns = computeMasonryColumns(viewportWidth, tileSpacing);
		const data = useMemo(
			() =>
				memes.map((meme) => ({
					id: meme.id,
					original: meme,
					width: meme.width ?? 200,
					height: meme.height ?? 200,
				})),
			[memes],
		);
		const itemKeys = useMemo(() => data.map((d) => d.id), [data]);
		const handleSelectKey = useCallback((itemKey: string) => {
			MemesPicker.trackMemeUsage(itemKey);
		}, []);
		return (
			<MasonryVirtualGrid
				data={data}
				itemKeys={itemKeys}
				columns={columns}
				tileSpacing={tileSpacing}
				viewportWidth={viewportWidth}
				viewportHeight={viewportHeight}
				scrollTop={scrollTop}
				onContentSizeChange={onContentSizeChange}
				checkSuspension={() => QuickSwitcher.isOpen}
				onSelectItemKey={handleSelectKey}
				tileKeyOf={(item) => item.id}
				tileHeightOf={(item, _index, laneWidth) => laneWidth * (item.height / item.width)}
				renderItem={({item, itemKey, coords, isFocused}) => (
					<MemeGridItem
						key={itemKey}
						meme={item.original}
						coords={coords}
						onClose={onClose}
						isFocused={isFocused}
						itemKey={itemKey}
						data-flx="channel.pickers.memes.memes-grid.meme-grid-item"
					/>
				)}
				data-flx="channel.pickers.memes.memes-grid.masonry-virtual-grid"
			/>
		);
	},
);
