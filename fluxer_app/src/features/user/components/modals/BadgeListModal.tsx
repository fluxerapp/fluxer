// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {handleExternalLinkClick} from '@app/features/ui/utils/NativeUtils';
import styles from '@app/features/user/components/modals/BadgeListModal.module.css';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const BADGES_DESCRIPTOR = msg({
	message: 'Badges',
	comment: 'Title of the window that lists every badge of a person or community.',
});

export interface BadgeListItem {
	readonly key: string;
	readonly imageUrl: string | null;
	readonly caption: string | null;
	readonly title: string;
	readonly lines: ReadonlyArray<string>;
	readonly href: string | null;
}

export const BadgeListModal = observer(function BadgeListModal({
	items,
	'data-flx': dataFlx = 'user.badge-list-modal',
}: {
	readonly items: ReadonlyArray<BadgeListItem>;
	readonly 'data-flx'?: string;
}) {
	const {i18n} = useLingui();
	return (
		<Modal.Root size="small" centered data-flx={`${dataFlx}.modal-root`}>
			<Modal.Header title={i18n._(BADGES_DESCRIPTOR)} data-flx={`${dataFlx}.modal-header`} />
			<Modal.Content data-flx={`${dataFlx}.modal-content`}>
				<ul className={styles.list} data-flx={`${dataFlx}.list`}>
					{items.map((item) => {
						const body = (
							<>
								{item.imageUrl !== null ? (
									<img
										src={item.imageUrl}
										alt=""
										loading="lazy"
										decoding="async"
										className={styles.image}
										data-flx={`${dataFlx}.image`}
									/>
								) : (
									<span className={styles.caption} aria-hidden="true" data-flx={`${dataFlx}.caption`}>
										{item.caption}
									</span>
								)}
								<span className={styles.text} data-flx={`${dataFlx}.text`}>
									<span className={styles.title} data-flx={`${dataFlx}.title`}>
										{item.title}
									</span>
									{item.lines.map((line) => (
										<span key={line} className={styles.line} data-flx={`${dataFlx}.line`}>
											{line}
										</span>
									))}
								</span>
							</>
						);
						const href = item.href;
						return (
							<li key={item.key} data-flx={`${dataFlx}.item`}>
								{href ? (
									<a
										href={href}
										target="_blank"
										rel="noopener noreferrer"
										className={styles.row}
										onClick={(event) => handleExternalLinkClick(event, href)}
										data-flx={`${dataFlx}.link`}
									>
										{body}
									</a>
								) : (
									<div className={styles.row} data-flx={`${dataFlx}.row`}>
										{body}
									</div>
								)}
							</li>
						);
					})}
				</ul>
			</Modal.Content>
		</Modal.Root>
	);
});
