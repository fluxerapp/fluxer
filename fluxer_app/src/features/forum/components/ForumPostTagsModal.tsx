// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import Channels from '@app/features/channel/state/Channels';
import * as ForumCommands from '@app/features/forum/commands/ForumCommands';
import {ForumTagPicker} from '@app/features/forum/components/ForumTagPicker';
import {getPostForum} from '@app/features/forum/utils/ForumChannelUtils';
import {reportForumError} from '@app/features/forum/utils/ForumErrors';
import * as D from '@app/features/forum/utils/ForumMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useEffect, useState} from 'react';

export const ForumPostTagsModal = observer(({postId}: {postId: string}) => {
	const {i18n} = useLingui();
	const post = Channels.getChannel(postId);
	const forum = post ? getPostForum(post) : undefined;
	const [selected, setSelected] = useState<Array<string>>(() => [...(post?.appliedTags ?? [])]);
	const [submitting, setSubmitting] = useState(false);
	const missing = !post || !forum;
	useEffect(() => {
		if (missing) ModalCommands.pop();
	}, [missing]);
	if (!post || !forum) return null;
	const handleSave = async () => {
		setSubmitting(true);
		try {
			await ForumCommands.setPostTags(post, selected);
			ModalCommands.pop();
		} catch (error) {
			reportForumError(i18n, error);
		} finally {
			setSubmitting(false);
		}
	};
	return (
		<Modal.Root size="small" centered data-flx="forum.forum-post-tags-modal.modal-root">
			<Modal.Header title={i18n._(D.EDIT_TAGS_DESCRIPTOR)} data-flx="forum.forum-post-tags-modal.modal-header" />
			<Modal.Content data-flx="forum.forum-post-tags-modal.modal-content">
				<ForumTagPicker
					forum={forum}
					selected={selected}
					onChange={setSelected}
					data-flx="forum.forum-post-tags-modal.forum-tag-picker.set-selected"
				/>
			</Modal.Content>
			<Modal.Footer data-flx="forum.forum-post-tags-modal.modal-footer">
				<Button onClick={ModalCommands.pop} variant="secondary" data-flx="forum.forum-post-tags-modal.button.pop">
					{i18n._(D.CANCEL_DESCRIPTOR)}
				</Button>
				<Button onClick={handleSave} submitting={submitting} data-flx="forum.forum-post-tags-modal.button.save">
					{i18n._(D.SAVE_DESCRIPTOR)}
				</Button>
			</Modal.Footer>
		</Modal.Root>
	);
});
