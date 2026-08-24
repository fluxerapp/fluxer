// SPDX-License-Identifier: AGPL-3.0-or-later

import type React from 'react';

type ReactionImageProps = Omit<React.ImgHTMLAttributes<HTMLImageElement>, 'alt' | 'src'> & {
	src: string;
	alt: string;
};

export const ReactionImage: React.FC<ReactionImageProps> = ({src, alt, loading = 'eager', ...props}) => {
	return <img src={src} alt={alt} loading={loading} data-flx="messaging.reaction-image.img" {...props} />;
};
