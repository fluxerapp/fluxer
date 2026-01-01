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

import type {EmailTranslations} from '../types';

export const vi: EmailTranslations = {
	passwordReset: {
		subject: 'Đặt lại mật khẩu Fluxer của bạn',
		body: `Xin chào {username},

Bạn đã yêu cầu đặt lại mật khẩu cho tài khoản Fluxer của mình. Vui lòng nhấn vào liên kết bên dưới để đặt mật khẩu mới:

{resetUrl}

Nếu bạn không yêu cầu đặt lại mật khẩu, bạn có thể bỏ qua email này.

Liên kết này sẽ hết hạn sau 1 giờ.

- Đội ngũ Fluxer`,
	},
	emailVerification: {
		subject: 'Xác minh địa chỉ email Fluxer của bạn',
		body: `Xin chào {username},

Vui lòng xác minh địa chỉ email cho tài khoản Fluxer của bạn bằng cách nhấn vào liên kết bên dưới:

{verifyUrl}

Nếu bạn không tạo tài khoản Fluxer, bạn có thể bỏ qua email này.

Liên kết này sẽ hết hạn sau 24 giờ.

- Đội ngũ Fluxer`,
	},
	ipAuthorization: {
		subject: 'Xác thực đăng nhập từ địa chỉ IP mới',
		body: `Xin chào {username},

Chúng tôi phát hiện một nỗ lực đăng nhập vào tài khoản Fluxer của bạn từ địa chỉ IP mới:

Địa chỉ IP: {ipAddress}
Vị trí: {location}

Nếu đây là bạn, hãy nhấn vào liên kết bên dưới để xác thực IP này:

{authUrl}

Nếu bạn không cố gắng đăng nhập, hãy thay đổi mật khẩu ngay lập tức.

Liên kết xác thực này sẽ hết hạn sau 30 phút.

- Đội ngũ Fluxer`,
	},
	accountDisabledSuspicious: {
		subject: 'Tài khoản Fluxer của bạn đã bị vô hiệu hóa tạm thời',
		body: `Xin chào {username},

Tài khoản Fluxer của bạn đã bị vô hiệu hóa tạm thời do hoạt động bất thường.

{reason, select,
	null {}
	other {Lý do: {reason}

}}Để khôi phục quyền truy cập, bạn cần đặt lại mật khẩu:

{forgotUrl}

Sau khi đặt lại mật khẩu, bạn có thể đăng nhập lại.

Nếu bạn tin rằng đây là nhầm lẫn, vui lòng liên hệ với đội hỗ trợ của chúng tôi.

- Đội An Toàn Fluxer`,
	},
	accountTempBanned: {
		subject: 'Tài khoản Fluxer của bạn đã bị tạm đình chỉ',
		body: `Xin chào {username},

Tài khoản Fluxer của bạn đã bị tạm đình chỉ vì vi phạm Điều khoản Dịch vụ hoặc Hướng dẫn Cộng đồng của chúng tôi.

Thời gian đình chỉ: {durationHours, plural,
	=1 {1 giờ}
	other {# giờ}
}
Đình chỉ đến: {bannedUntil, date, full} {bannedUntil, time, short}
{reason, select,
	null {}
	other {Lý do: {reason}}
}

Trong thời gian này, bạn sẽ không thể truy cập tài khoản của mình.

Chúng tôi khuyến nghị bạn xem lại:
- Điều khoản Dịch vụ: {termsUrl}
- Hướng dẫn Cộng đồng: {guidelinesUrl}

Nếu bạn tin rằng quyết định này không chính xác hoặc không công bằng, bạn có thể gửi đơn khiếu nại đến appeals@fluxer.app từ địa chỉ email này.  
Hãy giải thích rõ lý do tại sao bạn tin rằng quyết định này sai. Chúng tôi sẽ xem xét và phản hồi.

- Đội An Toàn Fluxer`,
	},
	accountScheduledDeletion: {
		subject: 'Tài khoản Fluxer của bạn đã được lên lịch xóa',
		body: `Xin chào {username},

Tài khoản Fluxer của bạn đã được lên lịch xóa vĩnh viễn vì vi phạm Điều khoản Dịch vụ hoặc Hướng dẫn Cộng đồng.

Ngày xóa dự kiến: {deletionDate, date, full} {deletionDate, time, short}
{reason, select,
	null {}
	other {Lý do: {reason}}
}

Đây là một biện pháp nghiêm trọng. Tất cả dữ liệu tài khoản của bạn sẽ bị xóa vĩnh viễn vào ngày đã định.

Chúng tôi khuyến nghị bạn xem lại:
- Điều khoản Dịch vụ: {termsUrl}
- Hướng dẫn Cộng đồng: {guidelinesUrl}

QUY TRÌNH KHIẾU NẠI:
Nếu bạn tin rằng quyết định này không đúng hoặc không công bằng, bạn có 30 ngày để gửi đơn khiếu nại đến appeals@fluxer.app từ email này.

Đơn khiếu nại nên bao gồm:
- Giải thích rõ vì sao bạn tin quyết định là sai
- Bất kỳ thông tin hoặc bằng chứng liên quan

Một thành viên Đội An Toàn Fluxer sẽ xem xét đơn khiếu nại của bạn và có thể hoãn việc xóa cho đến khi có quyết định cuối cùng.

- Đội An Toàn Fluxer`,
	},
	selfDeletionScheduled: {
		subject: 'Việc xóa tài khoản Fluxer của bạn đã được lên lịch',
		body: `Xin chào {username},

Chúng tôi rất tiếc khi thấy bạn rời đi! Việc xóa tài khoản Fluxer của bạn đã được lên lịch.

Ngày xóa dự kiến: {deletionDate, date, full} {deletionDate, time, short}

QUAN TRỌNG: Bạn có thể hủy việc xóa này bất cứ lúc nào trước {deletionDate, date, full} {deletionDate, time, short} chỉ bằng cách đăng nhập lại vào tài khoản.

TRƯỚC KHI BẠN RỜI ĐI:
Bảng Điều Khiển Quyền Riêng Tư cho phép bạn:
- Xóa tin nhắn của mình trên nền tảng
- Xuất dữ liệu quan trọng trước khi rời đi

Lưu ý: Sau khi tài khoản bị xóa, bạn sẽ không thể xóa tin nhắn nữa. Nếu bạn muốn xóa chúng, hãy thực hiện trước khi quá trình xóa hoàn tất.

Nếu bạn thay đổi ý định, chỉ cần đăng nhập lại để hủy việc xóa.

- Đội ngũ Fluxer`,
	},
	inactivityWarning: {
		subject: 'Tài khoản Fluxer của bạn sẽ bị xóa do không hoạt động',
		body: `Xin chào {username},

Chúng tôi nhận thấy bạn đã không đăng nhập vào tài khoản Fluxer của mình hơn 2 năm.

Lần đăng nhập cuối: {lastActiveDate, date, full} {lastActiveDate, time, short}

Theo chính sách lưu trữ dữ liệu của chúng tôi, các tài khoản không hoạt động sẽ được lên lịch xóa tự động.

Ngày xóa dự kiến: {deletionDate, date, full} {deletionDate, time, short}

CÁCH GIỮ TÀI KHOẢN CỦA BẠN:
Chỉ cần đăng nhập vào {loginUrl} trước ngày xóa để hủy quá trình tự động này.

NẾU BẠN KHÔNG ĐĂNG NHẬP:
- Tài khoản và toàn bộ dữ liệu của bạn sẽ bị xóa vĩnh viễn
- Tin nhắn của bạn sẽ được ẩn danh (“Người dùng đã xóa”)
- Hành động này là không thể hoàn tác

MUỐN XÓA TIN NHẮN CỦA BẠN?
Hãy đăng nhập và sử dụng Bảng Điều Khiển Quyền Riêng Tư trước khi tài khoản bị xóa.

Hy vọng sẽ được gặp lại bạn trên Fluxer!

- Đội ngũ Fluxer`,
	},
	harvestCompleted: {
		subject: 'Xuất dữ liệu Fluxer của bạn đã sẵn sàng',
		body: `Xin chào {username},

Quá trình xuất dữ liệu của bạn đã hoàn tất và sẵn sàng để tải xuống!

Tóm tắt xuất dữ liệu:
- Tổng số tin nhắn: {totalMessages, number}
- Kích thước tệp: {fileSizeMB} MB
- Định dạng: Tệp ZIP bao gồm các tệp JSON

Tải xuống dữ liệu của bạn: {downloadUrl}

LƯU Ý: Liên kết này sẽ hết hạn vào {expiresAt, date, full} {expiresAt, time, short}

Gói dữ liệu bao gồm:
- Tất cả tin nhắn của bạn theo từng kênh
- Siêu dữ liệu kênh
- Hồ sơ và thông tin tài khoản của bạn
- Thành viên guild và cài đặt
- Phiên đăng nhập và thông tin bảo mật

Dữ liệu được cung cấp dưới định dạng JSON để dễ dàng phân tích.

Nếu bạn có bất kỳ câu hỏi nào, vui lòng liên hệ support@fluxer.app

- Đội ngũ Fluxer`,
	},
	unbanNotification: {
		subject: 'Tài khoản Fluxer của bạn đã được gỡ khóa',
		body: `Xin chào {username},

Tin vui! Việc đình chỉ tài khoản Fluxer của bạn đã được gỡ bỏ.

Lý do: {reason}

Bạn có thể đăng nhập lại và tiếp tục sử dụng Fluxer.

- Đội An Toàn Fluxer`,
	},
	scheduledDeletionNotification: {
		subject: 'Tài khoản Fluxer của bạn đã được lên lịch xóa',
		body: `Xin chào {username},

Tài khoản Fluxer của bạn đã được lên lịch để xóa vĩnh viễn.

Ngày xóa: {deletionDate, date, full} {deletionDate, time, short}
Lý do: {reason}

Đây là một biện pháp nghiêm trọng. Tài khoản của bạn sẽ bị xóa hoàn toàn vào ngày trên.

Nếu bạn tin rằng việc này là sai, bạn có thể gửi khiếu nại đến appeals@fluxer.app

- Đội An Toàn Fluxer`,
	},
	giftChargebackNotification: {
		subject: 'Quà tặng Fluxer Premium của bạn đã bị thu hồi',
		body: `Xin chào {username},

Chúng tôi xin thông báo rằng quà tặng Fluxer Premium mà bạn đã kích hoạt đã bị thu hồi do tranh chấp thanh toán (chargeback) từ người mua ban đầu.

Các quyền lợi Premium đã bị xóa khỏi tài khoản của bạn.

Nếu bạn có thắc mắc, vui lòng liên hệ support@fluxer.app

- Đội ngũ Fluxer`,
	},
	reportResolved: {
		subject: 'Báo cáo Fluxer của bạn đã được xem xét',
		body: `Xin chào {username},

Báo cáo của bạn (ID: {reportId}) đã được đội ngũ An Toàn Fluxer xem xét.

Phản hồi từ đội ngũ:
{publicComment}

Cảm ơn bạn đã đóng góp để giữ Fluxer an toàn cho cộng đồng. Chúng tôi trân trọng sự đóng góp của bạn.

Nếu bạn có câu hỏi hoặc lo ngại, hãy liên hệ safety@fluxer.app

- Đội An Toàn Fluxer`,
	},
	dsaReportVerification: {
		subject: 'Xác minh email của bạn cho báo cáo DSA',
		body: `Xin chào,

Sử dụng mã xác minh sau để gửi báo cáo Đạo luật Dịch vụ Kỹ thuật số của bạn trên Fluxer:

{code}

Mã này sẽ hết hạn vào {expiresAt, date, full} {expiresAt, time, short}.

Nếu bạn không yêu cầu điều này, vui lòng bỏ qua email này.

- Đội An Toàn Fluxer`,
	},
	registrationApproved: {
		subject: 'Đăng ký Fluxer của bạn đã được phê duyệt',
		body: `Xin chào {username},

Tin vui! Việc đăng ký Fluxer của bạn đã được phê duyệt.

Bạn có thể đăng nhập ứng dụng Fluxer tại:
{channelsUrl}

Chào mừng bạn đến với cộng đồng Fluxer!

- Đội ngũ Fluxer`,
	},
	emailChangeRevert: {
		subject: 'Email Fluxer của bạn đã được thay đổi',
		body: `Xin chào {username},

Email tài khoản Fluxer của bạn đã được thay đổi thành {newEmail}.

Nếu bạn tự thay đổi, bạn không cần làm gì thêm. Nếu không, hãy hoàn tác và bảo vệ tài khoản bằng liên kết này:

{revertUrl}

Việc này sẽ khôi phục email trước đó, đăng xuất bạn khỏi mọi phiên, xóa số điện thoại liên kết, tắt MFA và yêu cầu mật khẩu mới.

- Đội ngũ An ninh Fluxer`,
	},
};
