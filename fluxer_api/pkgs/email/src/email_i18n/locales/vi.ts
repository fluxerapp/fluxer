// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineEmailI18nLocaleMessages} from '../EmailI18nMessages';

const EMAIL_I18N_VI_MESSAGES = defineEmailI18nLocaleMessages({
	"account_disabled_suspicious": {
		"subject": "Tài khoản {product_name} của bạn đã bị vô hiệu hóa tạm thời",
		"body": "Xin chào {username},\n\nChúng tôi đã tạm thời vô hiệu hóa tài khoản {product_name} của bạn vì phát hiện hoạt động đáng ngờ.\n\n{reason, select,\n  null {}\n  other {Lý do: {reason}}\n}\n\nĐể truy cập lại tài khoản, bạn cần đặt lại mật khẩu:\n\n{forgotUrl}\n\nSau khi đặt lại mật khẩu, bạn sẽ có thể đăng nhập lại.\n\nNếu bạn cho rằng đây là lỗi, vui lòng liên hệ đội ngũ hỗ trợ của chúng tôi.\n\n– Đội ngũ An toàn {product_name}"
	},
	"account_scheduled_deletion": {
		"subject": "Tài khoản {product_name} của bạn sẽ bị xóa vĩnh viễn",
		"body": "Xin chào {username},\n\nTài khoản {product_name} của bạn đã được lên lịch xóa vĩnh viễn do vi phạm Điều khoản dịch vụ hoặc Nguyên tắc cộng đồng của chúng tôi.\n\nLịch xóa: {deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {Lý do: {reason}}\n}\n\nĐây là một hành động thực thi nghiêm trọng. Dữ liệu tài khoản của bạn sẽ bị xóa vĩnh viễn vào ngày đã lên lịch.\n\nVui lòng xem lại:\n- Điều khoản dịch vụ: {termsUrl}\n- Nguyên tắc cộng đồng: {guidelinesUrl}\n\nQuy trình kháng nghị:\nNếu bạn cho rằng quyết định này không chính xác hoặc không hợp lý, bạn có 60 ngày để gửi kháng nghị. Gửi email đến {appeals_email} từ địa chỉ email này.\n\nTrong kháng nghị của bạn:\n- Giải thích rõ ràng lý do bạn cho rằng quyết định này không chính xác hoặc không hợp lý\n- Cung cấp bất kỳ bằng chứng hoặc ngữ cảnh liên quan nào\n\nMột thành viên của Đội ngũ An toàn {product_name} sẽ xem xét kháng nghị của bạn và có thể tạm dừng việc xóa đang chờ xử lý cho đến khi có quyết định cuối cùng.\n\n– Đội ngũ An toàn {product_name}"
	},
	"account_temp_banned": {
		"subject": "Tài khoản {product_name} của bạn đã bị tạm ngưng",
		"body": "Xin chào {username},\n\nTài khoản {product_name} của bạn đã bị tạm ngưng do vi phạm Điều khoản dịch vụ hoặc Nguyên tắc cộng đồng của chúng tôi.\n\nThời gian: {durationHours, plural,\n  =1 {1 giờ}\n  other {# giờ}\n}\nBị tạm ngưng cho đến: {bannedUntil, date, full} {bannedUntil, time, short}\n\n{reason, select,\n  null {}\n  other {Lý do: {reason}}\n}\n\nTrong thời gian này, bạn sẽ không thể truy cập tài khoản của mình.\n\nVui lòng xem lại:\n- Điều khoản dịch vụ: {termsUrl}\n- Nguyên tắc cộng đồng: {guidelinesUrl}\n\nNếu bạn cho rằng quyết định này không chính xác hoặc không hợp lý, bạn có thể gửi kháng nghị. Gửi email đến {appeals_email} từ địa chỉ email này và giải thích rõ ràng lý do bạn cho rằng quyết định đó không chính xác. Chúng tôi sẽ xem xét kháng nghị của bạn và phản hồi bằng quyết định của chúng tôi.\n\n– Đội ngũ An toàn {product_name}"
	},
	"donation_confirmation": {
		"subject": "Cảm ơn bạn đã ủng hộ {product_name}",
		"body": "Xin chào,\n\nCảm ơn bạn đã ủng hộ {product_name}! {interval, select,\n  month {Đóng góp định kỳ}\n  year {Đóng góp định kỳ}\n  other {Khoản đóng góp một lần}\n} của bạn đã được {interval, select,\n  month {thiết lập}\n  year {thiết lập}\n  other {xử lý}\n} thành công.\n\nChi tiết ủng hộ:\nSố tiền: {amount} {currency} {interval, select,\n  month {mỗi tháng}\n  year {mỗi năm}\n  other {}\n}\n\nStripe sẽ gửi email cho bạn một biên lai riêng kèm theo hóa đơn PDF của bạn trong thời gian ngắn. Biên lai này bao gồm tất cả các chi tiết thanh toán và có thể được sử dụng cho mục đích thuế.\n\nBạn có thể xem lịch sử ủng hộ, tải xuống hóa đơn, {interval, select,\n  month {và quản lý hoặc hủy đăng ký}\n  year {và quản lý hoặc hủy đăng ký}\n  other {và quản lý các khoản ủng hộ trong tương lai}\n} bất cứ lúc nào bằng cách sử dụng liên kết này:\n\n{manageUrl}\n\nSự hỗ trợ của bạn giúp {product_name} tiếp tục hoạt động. Cảm ơn bạn!\n\n– Đội ngũ {product_name}"
	},
	"donation_magic_link": {
		"subject": "Quản lý các khoản ủng hộ {product_name} của bạn",
		"body": "Xin chào,\n\nNhấp vào liên kết bên dưới để truy cập cổng thông tin người ủng hộ của bạn:\n\n{manageUrl}\n\nTrong cổng thông tin, bạn có thể quản lý đăng ký, tải xuống hóa đơn và xem lịch sử ủng hộ của mình.\n\nLiên kết này hết hạn vào {expiresAt, date, full} lúc {expiresAt, time, short}.\n\nNếu bạn không yêu cầu liên kết này, bạn có thể bỏ qua email này.\n\n– Đội ngũ {product_name}"
	},
	"dsa_report_verification": {
		"subject": "Xác minh email của bạn cho báo cáo DSA",
		"body": "Xin chào,\n\nSử dụng mã xác minh bên dưới để gửi báo cáo Đạo luật Dịch vụ Kỹ thuật số của bạn trên {product_name}:\n\n{code}\n\nMã này hết hạn vào {expiresAt, date, full} lúc {expiresAt, time, short}.\n\nNếu bạn không yêu cầu mã này, bạn có thể bỏ qua email này.\n\n– Đội ngũ An toàn {product_name}"
	},
	"email_change_new": {
		"subject": "Xác minh email {product_name} mới của bạn",
		"body": "Xin chào {username},\n\nNhập mã này vào ứng dụng để xác minh email {product_name} mới của bạn:\n\n{code}\n\nMã này hết hạn vào {expiresAt, date, full} lúc {expiresAt, time, short}.\n\nNếu bạn không yêu cầu mã này, bạn có thể bỏ qua email này.\n\n– Đội ngũ {product_name}"
	},
	"email_change_original": {
		"subject": "Xác nhận thay đổi email của bạn trên {product_name}",
		"body": "Xin chào {username},\n\nChúng tôi đã nhận được yêu cầu thay đổi địa chỉ email trên tài khoản {product_name} của bạn.\n\nĐể xác nhận thay đổi này, hãy nhập mã này vào ứng dụng:\n\n{code}\n\nMã này hết hạn vào {expiresAt, date, full} lúc {expiresAt, time, short}.\n\nNếu bạn không yêu cầu thay đổi này, vui lòng bảo mật tài khoản của bạn ngay lập tức.\n\n– Đội ngũ {product_name}"
	},
	"email_change_revert": {
		"subject": "Email của bạn trên {product_name} đã được thay đổi",
		"body": "Xin chào {username},\n\nĐịa chỉ email trên tài khoản {product_name} của bạn đã được thay đổi thành {newEmail}.\n\nNếu bạn đã thực hiện thay đổi này, bạn không cần thực hiện hành động nào. Nếu không, bạn có thể hoàn tác thay đổi và bảo mật tài khoản của mình bằng cách sử dụng liên kết này:\n\n{revertUrl}\n\nThao tác này sẽ khôi phục email trước đó của bạn, đăng xuất bạn khỏi mọi nơi, xóa số điện thoại đã liên kết, tắt MFA và yêu cầu bạn đặt mật khẩu mới.\n\n– Đội ngũ An toàn {product_name}"
	},
	"email_verification": {
		"subject": "Xác minh địa chỉ email của bạn trên {product_name}",
		"body": "Xin chào {username},\n\nVui lòng xác minh địa chỉ email cho tài khoản {product_name} của bạn bằng cách nhấp vào liên kết bên dưới:\n\n{verifyUrl}\n\nNếu bạn không tạo tài khoản {product_name}, bạn có thể bỏ qua email này.\n\nLiên kết này có hiệu lực trong 24 giờ.\n\n– Đội ngũ {product_name}"
	},
	"gift_chargeback_notification": {
		"subject": "Các đặc quyền từ quà tặng bạn đã đổi đã bị xóa",
		"body": "Xin chào {username},\n\nMã quà tặng bạn đã đổi ban đầu được thanh toán bởi người khác. Khoản thanh toán đó sau đó đã bị đảo ngược (chargeback).\n\nVì lý do này, chúng tôi đã xóa các đặc quyền được thêm vào tài khoản của bạn khi bạn đổi quà tặng.\n\nNếu bạn cho rằng đây là lỗi, vui lòng liên hệ đội ngũ hỗ trợ của chúng tôi và cung cấp bất kỳ chi tiết nào bạn có về mã quà tặng và thời điểm bạn đổi mã đó.\n\n– Đội ngũ {product_name}"
	},
	"harvest_completed": {
		"subject": "Xuất dữ liệu {product_name} của bạn đã sẵn sàng để tải xuống",
		"body": "Xin chào {username},\n\nBản xuất dữ liệu của bạn đã sẵn sàng.\n\nLiên kết tải xuống:\n{downloadUrl}\n\nTin nhắn bao gồm: {totalMessages, number}\nKích thước tệp: {fileSizeMB, number} MB\n\nLiên kết này hết hạn vào {expiresAt, date, full} lúc {expiresAt, time, short}.\n\nNếu bạn không yêu cầu xuất dữ liệu này, vui lòng thay đổi mật khẩu ngay lập tức và liên hệ đội ngũ hỗ trợ của chúng tôi.\n\n– Đội ngũ {product_name}"
	},
	"inactivity_warning": {
		"subject": "Tài khoản {product_name} của bạn sẽ bị xóa do không hoạt động",
		"body": "Xin chào {username},\n\nChúng tôi chưa thấy bất kỳ hoạt động nào trên tài khoản {product_name} của bạn kể từ {lastActiveDate, date, full}.\n\nNếu bạn không đăng nhập trước {deletionDate, date, full} lúc {deletionDate, time, short}, tài khoản của bạn sẽ bị xóa vĩnh viễn do không hoạt động.\n\nĐăng nhập tại đây:\n{loginUrl}\n\nNếu bạn đã sử dụng {product_name} gần đây, vui lòng liên hệ đội ngũ hỗ trợ của chúng tôi ngay lập tức.\n\n– Đội ngũ {product_name}"
	},
	"ip_authorization": {
		"subject": "Cho phép đăng nhập từ địa chỉ IP mới",
		"body": "Xin chào {username},\n\nChúng tôi đã phát hiện một nỗ lực đăng nhập vào tài khoản {product_name} của bạn từ một địa chỉ IP mới:\n\nĐịa chỉ IP: {ipAddress}\nVị trí: {location}\n\nNếu đây là bạn, vui lòng cho phép địa chỉ IP này bằng cách nhấp vào liên kết bên dưới:\n\n{authUrl}\n\nNếu bạn không thực hiện nỗ lực đăng nhập, vui lòng thay đổi mật khẩu ngay lập tức.\n\nLiên kết này có hiệu lực trong 30 phút.\n\n– Đội ngũ {product_name}"
	},
	"password_change_verification": {
		"subject": "Xác nhận thay đổi mật khẩu của bạn trên {product_name}",
		"body": "Xin chào {username},\n\nChúng tôi đã nhận được yêu cầu thay đổi mật khẩu trên tài khoản {product_name} của bạn.\n\nĐể xác nhận thay đổi này, hãy nhập mã này vào ứng dụng:\n\n{code}\n\nMã này hết hạn lúc {expiresAt}.\n\nNếu bạn không yêu cầu thay đổi này, ai đó có thể đã truy cập vào tài khoản của bạn. Thay đổi mật khẩu ngay lập tức và bật xác thực hai yếu tố.\n\n– Đội ngũ {product_name}"
	},
	"password_reset": {
		"subject": "Đặt lại mật khẩu của bạn trên {product_name}",
		"body": "Xin chào {username},\n\nBạn đã yêu cầu đặt lại mật khẩu {product_name}. Sử dụng liên kết bên dưới để đặt mật khẩu mới:\n\n{resetUrl}\n\nNếu bạn không yêu cầu điều này, bạn có thể bỏ qua email này.\n\nLiên kết này có hiệu lực trong 1 giờ.\n\n– Đội ngũ {product_name}"
	},
	"registration_approved": {
		"subject": "Đăng ký {product_name} của bạn đã được chấp thuận",
		"body": "Xin chào {username},\n\nTin tốt: đăng ký {product_name} của bạn đã được chấp thuận.\n\nBây giờ bạn có thể đăng nhập vào ứng dụng {product_name} tại đây:\n{channelsUrl}\n\nChào mừng bạn đến với cộng đồng {product_name}.\n\n– Đội ngũ {product_name}"
	},
	"report_resolved": {
		"subject": "Báo cáo {product_name} của bạn đã được xem xét",
		"body": "Xin chào {username},\n\nBáo cáo của bạn (ID: {reportId}) đã được Đội ngũ An toàn của chúng tôi xem xét.{hasComment, select, yes {\n\nPhản hồi từ Đội ngũ An toàn:\n{publicComment}} other {}}\n\nCảm ơn bạn đã giúp giữ cho {product_name} an toàn cho mọi người. Chúng tôi xem xét tất cả các báo cáo một cách nghiêm túc và đánh giá cao sự đóng góp của bạn cho cộng đồng.\n\nNếu bạn có bất kỳ câu hỏi hoặc lo ngại nào về kết quả này, vui lòng liên hệ {safety_email}.\n\n– Đội ngũ An toàn {product_name}"
	},
	"scheduled_deletion_notification": {
		"subject": "Tài khoản {product_name} của bạn sẽ bị xóa vĩnh viễn",
		"body": "Xin chào {username},\n\nTài khoản {product_name} của bạn đã được lên lịch xóa vĩnh viễn.\n\nLịch xóa: {deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {Lý do: {reason}}\n}\n\nĐây là một hành động thực thi nghiêm trọng. Dữ liệu tài khoản của bạn sẽ bị xóa vĩnh viễn vào ngày đã lên lịch.\n\nNếu bạn cho rằng quyết định này không chính xác, bạn có thể gửi kháng nghị. Gửi email đến {appeals_email} từ địa chỉ email này.\n\n– Đội ngũ An toàn {product_name}"
	},
	"self_deletion_scheduled": {
		"subject": "Xóa tài khoản {product_name} của bạn đã được lên lịch",
		"body": "Xin chào {username},\n\nBạn đã yêu cầu xóa tài khoản {product_name} của mình. Tài khoản của bạn được lên lịch xóa vĩnh viễn vào:\n\n{deletionDate, date, full} lúc {deletionDate, time, short}\n\nNếu bạn không yêu cầu điều này, hãy đăng nhập vào tài khoản của bạn để hủy xóa. Chúng tôi cũng khuyên bạn nên thay đổi mật khẩu để bảo mật tài khoản của mình.\n\n– Đội ngũ {product_name}"
	},
	"unban_notification": {
		"subject": "Tài khoản {product_name} của bạn đã được bỏ tạm ngưng",
		"body": "Xin chào {username},\n\nTin tốt: tài khoản {product_name} của bạn đã được bỏ tạm ngưng.\n\n{reason, select,\n  null {}\n  other {Lý do: {reason}}\n}\n\nBây giờ bạn có thể đăng nhập lại và tiếp tục sử dụng {product_name} như bình thường.\n\n– Đội ngũ An toàn {product_name}"
	}
});

export default EMAIL_I18N_VI_MESSAGES;
