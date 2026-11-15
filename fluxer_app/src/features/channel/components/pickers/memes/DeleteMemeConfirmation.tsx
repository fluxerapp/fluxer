// SPDX-License-Identifier: AGPL-3.0-or-later
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import * as FavoriteMemeCommands from '@app/features/expressions/commands/FavoriteMemeCommands';
import type {FavoriteMeme} from '@app/features/expressions/models/FavoriteMeme';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
export const DELETE_SAVED_MEDIA_DESCRIPTOR = msg({
	message: 'Delete saved media',
	comment: 'Title of the destructive delete confirmation alert in the saved media (memes) picker.',
});
const ARE_YOU_SURE_YOU_WANT_TO_DELETE_THIS_DESCRIPTOR = msg({
	message: 'Delete "{memeName}"? Can\'t be undone.',
	comment:
		'Body of the destructive delete confirmation alert in the saved media picker. memeName is the user-chosen name. Keep quotation marks.',
});
const DELETE_DESCRIPTOR = msg({
	message: 'Delete',
	comment: 'Confirm button label on the destructive delete-saved-media alert.',
});
export function buildDeleteMemeConfirmation(i18n: I18n, meme: FavoriteMeme) {
	return (
		<ConfirmModal
			title={i18n._(DELETE_SAVED_MEDIA_DESCRIPTOR)}
			description={i18n._(ARE_YOU_SURE_YOU_WANT_TO_DELETE_THIS_DESCRIPTOR, {memeName: meme.name})}
			primaryText={i18n._(DELETE_DESCRIPTOR)}
			primaryVariant="danger"
			onPrimary={() => {
				FavoriteMemeCommands.deleteFavoriteMeme(i18n, meme.id);
			}}
			data-flx="channel.pickers.memes.delete-meme-confirmation.confirm-modal"
		/>
	);
}
