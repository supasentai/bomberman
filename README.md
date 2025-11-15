# Bomberman Multiplayer bằng Haskell

Một dự án game Bomberman multiplayer thời gian thực được xây dựng hoàn toàn bằng ngôn
ngữ lập trình Haskell. Dự án này sử dụng kiến trúc Client-Server, áp dụng Software
Transactional Memory (STM) để quản lý đồng thời, và thư viện Gloss để hiển thị
đồ họa.

*(Bạn nên thay thế dòng này bằng một ảnh chụp màn hình hoặc GIF của trò chơi)*

## Tính năng

  * **Lối chơi Multiplayer:** Hỗ trợ nhiều người chơi kết nối đến một máy chủ trung tâm.
  * **Hai Chế độ chơi:**
      * `Co-op (P vs P)`: Người chơi đấu với nhau.
      * `1v1 (P vs AI)`: Người chơi đấu với đối thủ máy.
  * **Trí tuệ Nhân tạo (AI):** Cả AI cho Quái vật (di chuyển và săn đuổi) và AI cho
    Người chơi (có khả năng né bom, săn vật phẩm và tấn công).
  * **Vật phẩm (Power-ups):** Bao gồm BombUp, FlameUp, Shield và Chaos.
  * **Hệ thống Chat:** Người chơi có thể gửi tin nhắn cho nhau trong thời gian thực.
  * **Hệ thống Vật lý Game:** Xử lý nổ bom, nổ dây chuyền, phá hủy các vật cản (hộp).

## Kiến trúc Kỹ thuật

Đây là một dự án phức tạp tập trung vào việc áp dụng các mô hình lập trình
tiên tiến trong Haskell để giải quyết các vấnode bài toán của game thời gian thực.

### 1\. Mô hình Client-Server

Hệ thống được xây dựng trên kiến trúc Client-Server (máy chủ quyền uy):

  * **Server (`Server.hs`):** Là "bộ não" của trò chơi. Nó quản lý toàn bộ
    `GameState` (trạng thái game), xử lý logic, chạy AI, và là "nguồn chân lý
    duy nhất". Máy chủ sử dụng kiến trúc **"thread-per-client"**, tạo một luồng
    riêng (`clientHandler`) cho mỗi người chơi kết nối.
  * **Client (`NetworkedMain.hs`):** Là một "máy khách câm" (Dumb Client). Nó
    chịu trách nhiệm hiển thị đồ họa (dùng Gloss), bắt sự kiện (input người
    dùng) và gửi lên server. Nó không bao giờ tự ý thay đổi trạng thái game.

### 2\. Quản lý Đồng thời (Concurrency) và Đồng bộ hóa (STM)

Đây là công nghệ cốt lõi của máy chủ.

  * **Concurrency:** Máy chủ có nhiều luồng chạy đồng thời: một luồng `gameLoop`
    (cập nhật logic game 30 lần/giây) và nhiều luồng `clientHandler` (nhận lệnh
    từ người chơi).
  * **Synchronization (STM):** Để ngăn chặn `race condition` (xung đột dữ liệu)
    khi nhiều luồng cùng đọc/ghi vào `GameState`, dự án sử dụng
    **Software Transactional Memory (STM)**.
      * Toàn bộ `GameState` được bọc trong một `TVar` (`stateVar`).
      * Mọi hành động cập nhật (từ `gameLoop` hay `clientHandler`) đều phải được
        thực hiện bên trong một khối `atomically`.
      * STM tự động đảm bảo tính toàn vẹn dữ liệu, giải quyết xung đột và tránh
        hoàn toàn `deadlock` mà không cần dùng `lock` (khóa) truyền thống.

### 3\. Giao thức Mạng (Socket & Aeson)

  * **Socket:** Client và Server giao tiếp qua **Socket TCP** để đảm bảo tính
    tin cậy của dữ liệu.
  * **Aeson (JSON):** Thay vì gửi các gói tin nhỏ lẻ, máy chủ `encode` **toàn
    bộ** `GameState` thành một chuỗi **JSON** (dùng thư viện Aeson) và
    `broadcast` (phát sóng) đến tất cả client sau mỗi "tick". Client nhận
    chuỗi JSON này, `decode` nó trở lại cấu trúc `GameState`, và dùng nó để
    vẽ khung hình mới.

### 4\. Trí tuệ Nhân tạo (AI)

Hệ thống AI (`GameLogic.hs`) không chỉ di chuyển ngẫu nhiên mà sử dụng thuật
toán tìm đường và cây quyết định:

  * **Thuật toán:** Sử dụng **BFS (Breadth-First Search)** (triển khai trong
    hàm `findPath`) để tìm đường đi ngắn nhất.
  * **Cây Quyết định:** AI Player có một hệ thống ưu tiên rõ ràng:
    1.  **An toàn:** Kiểm tra `getDangerTiles` (ô nguy hiểm). Nếu đang gặp nguy,
        ưu tiên hàng đầu là chạy thoát (`findPathToSafety`).
    2.  **Tấn công:** Nếu an toàn, kiểm tra xem có `Box`, Quái vật, hoặc người
        chơi khác ở gần để đặt bom.
    3.  **Chiến thuật:** Nếu không, chủ động săn lùng vật phẩm hoặc người chơi
        khác (`aiStrategicHunt`).

### 5\. Hiển thị Đồ họa (Gloss & Nội suy)

  * **Graphics.Gloss:** Client sử dụng `playIO` của Gloss (thay vì `play` thuần
    túy) để cho phép các tác vụ I/O (như luồng `recvLoop` nhận dữ liệu mạng)
    chạy song song với vòng lặp vẽ.
  * **Nội suy (Interpolation):** Để che giấu độ trễ mạng và tránh hiện tượng "giật"
    (jitter), client sử dụng kỹ thuật nội suy đồ họa:
      * Nó duy trì `gameVar` (trạng thái logic "thật" từ server, ví dụ `(5, 5)`).
      * Nó duy trì `visualPlayers` (trạng thái "ảo" đang hiển thị, ví dụ
        `(4.8, 5.0)`).
      * Trong mỗi khung hình, hàm `updateFunc` sẽ di chuyển mượt mà trạng thái
        "ảo" *tiến về* trạng thái "thật" (dùng `moveTowards`), tạo ra chuyển
        động mượt mà.

## Cài đặt và Chạy thử

Dự án này sử dụng `stack` để quản lý dependencies và build.

1.  **Clone dự án:**

    ```bash
    git clone https://github.com/supasentai/bomberman
    cd bomberman
    ```

2.  **Build dự án:**

    ```bash
    stack build
    ```

3.  **Chạy Máy chủ (Server):**
    Mở một cửa sổ terminal và chạy:

    ```bash
    stack run bomberman-server
    ```

    Bạn sẽ thấy thông báo: `🔥 Server started at port 4242`

4.  **Chạy Máy khách (Client):**
    Mở một (hoặc nhiều) cửa sổ terminal khác và chạy:

    ```bash
    stack run bomberman-client
    ```

    Một cửa sổ game sẽ hiện lên, kết nối vào server.

## Cách chơi

  * **Tại Sảnh chờ (Lobby):**
      * Nhấn phím `1` để bắt đầu chế độ Co-op (P vs P).
      * Nhấn phím `2` để bắt đầu chế độ 1v1 (P vs AI).
  * **Trong Game:**
      * `W, A, S, D`: Di chuyển.
      * `B`: Đặt bom.
      * `Enter`: Nhấn để vào chế độ chat, gõ tin nhắn, rồi nhấn `Enter` lần
        nữa để gửi.

## Cấu trúc Dự án

```
.
├── assets/             # Chứa các tài nguyên ảnh .bmp (tường, bom, nhân vật...)
├── app/
│   ├── Server.hs         # Logic máy chủ chính, quản lý kết nối, STM, game loop.
│   └── NetworkedMain.hs  # Logic máy khách chính, khởi tạo Gloss, xử lý input.
├── src/
│   ├── GameLogic.hs      # "Bộ não": Luật chơi, xử lý nổ, va chạm, AI, BFS.
│   ├── Render.hs         # Các hàm "thuần túy" để vẽ mọi đối tượng trong game.
│   └── Types.hs          # Định nghĩa tất cả cấu trúc dữ liệu (GameState, Player, v.v.)
├── package.yaml        # Định nghĩa dependencies và executables.
└── stack.yaml          # Cấu hình build của Stack.
```
