// SPDX-License-Identifier: AGPL-3.0-or-later

import {useConfirmDeleteMyMessages} from '@app/features/guild/hooks/useConfirmDeleteMyMessages';
import {renderToStaticMarkup} from 'react-dom/server';
import {beforeEach, describe, expect, it, vi} from 'vitest';

const mocks = vi.hoisted(() => ({push: vi.fn(), toast: vi.fn(), error: vi.fn()}));
vi.mock('@lingui/react/macro', () => ({useLingui: () => ({i18n: {_: () => 'Delete my messages'}})}));
vi.mock('@app/features/platform/utils/AppLogger', () => ({
	Logger: class {
		error() {}
	},
}));
vi.mock('@app/features/channel/utils/ChannelMessageDescriptors', () => ({
	DELETE_MY_MESSAGES_DESCRIPTOR: {message: 'Delete my messages'},
}));
vi.mock('@app/features/app/components/dialogs/ConfirmModal', () => ({ConfirmModal: () => null}));
vi.mock('@app/features/app/components/alerts/GenericErrorModalCommands', () => ({showGenericErrorModal: mocks.error}));
vi.mock('@app/features/ui/commands/ModalCommands', () => ({
	push: mocks.push,
	modal: (render: () => unknown) => render,
}));
vi.mock('@app/features/ui/commands/ToastCommands', () => ({createToast: mocks.toast}));
let confirm: ReturnType<typeof useConfirmDeleteMyMessages>;
function Probe() {
	confirm = useConfirmDeleteMyMessages();
	return null;
}
beforeEach(() => {
	vi.clearAllMocks();
	renderToStaticMarkup(<Probe data-flx="guild.use-confirm-delete-my-messages-test.probe" />);
});
describe('message deletion confirmation', () => {
	it('waits for confirmation and successful deletion before showing success', async () => {
		let finish!: () => void;
		const run = vi.fn(
			() =>
				new Promise<void>((resolve) => {
					finish = resolve;
				}),
		);
		confirm({
			title: 'Delete?',
			description: 'Cannot undo',
			run,
			dataFlx: 'guild.delete.confirm-modal',
			toastText: 'Deleted',
			errorTitle: 'Failed',
			errorMessage: 'Try again',
		});
		expect(run).not.toHaveBeenCalled();
		const modal = mocks.push.mock.calls[0][0]();
		const pending = modal.props.onPrimary();
		expect(run).toHaveBeenCalledOnce();
		expect(mocks.toast).not.toHaveBeenCalled();
		finish();
		await pending;
		expect(mocks.toast).toHaveBeenCalledWith({type: 'success', children: 'Deleted'});
	});
	it('defers the error modal until the confirmation dismisses', async () => {
		const run = vi.fn().mockRejectedValue(new Error('server'));
		confirm({
			title: 'Delete?',
			description: 'Cannot undo',
			run,
			dataFlx: 'channel.delete.confirm-modal',
			toastText: 'Deleted',
			errorTitle: 'Channel failure',
			errorMessage: 'Try later',
		});
		await mocks.push.mock.calls[0][0]().props.onPrimary();
		expect(mocks.toast).not.toHaveBeenCalled();
		expect(mocks.error).toHaveBeenCalledWith({
			title: 'Channel failure',
			message: 'Try later',
			dataFlx: 'channel.delete.generic-error-modal',
			defer: true,
		});
	});
});
